module Main

import Data.Buffer
import Data.Buffer.Ext
import Control.Monad.Trans
import Control.Monad.Either
import Control.Monad.Maybe
import Node.HTTP.Client
import Node.HTTP.Server
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.URI
import TyTTP.HTTP
import TyTTP.HTTP.Consumer
import TyTTP.HTTP.Consumer.JSON
import TyTTP.HTTP.Producer.JSON
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search

import PG.Postgres
import PG.Promise
import PG.Util
import Debug.Trace

import Data.List.Quantifiers
import Generics.Derive
import JSON

%foreign """
node:lambda: (str) => { return {message: str, code: str, stack:""} }
"""
prim__from_string : String -> IO NodeError

-- TODO many many hacks
FromString NodeError where
  fromString x = unsafePerformIO $ prim__from_string x

%language ElabReflection

record Foo where
  constructor MkFoo
  bar : Int
  baz : Int

%runElab derive "Foo" [Generic, Meta, Show, Eq, RecordToJSON, RecordFromJSON]

data Country =
  MkCountry String Nat
Show Country where
  show (MkCountry x k) = "MkCountry \{x} \{show k}"

fooFromRow : (us : List Universe) -> (RowU us) -> Maybe Foo
fooFromRow ([Num, Num]) ([x, y]) = Just $ MkFoo (cast x) (cast y)
fooFromRow (x :: _) _ = Nothing
fooFromRow ([]) _ = Nothing

countryFromRow : (us : List Universe) -> (RowU us) -> Maybe Country
countryFromRow ([Str, Num]) ([v, x]) = Just $ MkCountry v (cast x)
countryFromRow (x :: _) _ = Nothing
countryFromRow ([]) _ = Nothing

tryFoo : Maybe (us ** Table us) -> Maybe (List Foo)
tryFoo Nothing = Nothing
tryFoo (Just (MkDPair fst snd)) = traverse (fooFromRow fst) snd

tryCountry : Maybe (us ** Table us) -> Maybe (List Country)
tryCountry Nothing = Nothing
tryCountry (Just (MkDPair fst snd)) = traverse (countryFromRow fst) snd

fooToQuery : Foo -> String
fooToQuery (MkFoo bar baz) = "(\{show bar},\{show baz})"

insertFoo : Pool -> Foo -> PG.Promise.Promise e IO ()
insertFoo pool foo = do
  -- BAD: vulnerable to SQL injection
  -- need to work out how to pass a HList to the FFI
  ignore $ query pool "INSERT INTO foo(bar,baz) VALUES \{fooToQuery foo};"

getFoos : FromString e => Pool -> PG.Promise.Promise e IO (List Foo)
getFoos pool = do
  b <- query pool "SELECT bar,baz FROM foo"
  foos <- lift $ getAll b
  ls <- lift $ tryFoo foos
  case ls of
       Nothing => reject "Error: got nothing"
       (Just cs) => pure $ trace (show cs) cs

getCountries : FromString e => Pool -> PG.Promise.Promise e IO (List Country)
getCountries pool = do
  b <- query pool "SELECT country,total FROM board"
  countries <- lift $ getAll b
  ls <- lift $ tryCountry countries
  case ls of
       Nothing => reject "Error: got nothing"
       (Just cs) => pure $ trace (show cs) cs

transform : PG.Promise.Promise e m a -> Core.Promise.Promise e m a
transform x = MkPromise $ \cb => do
  resolve x
    (\a => cb.onSucceded a)
    (\e => cb.onFailed e)

main : IO ()
main = eitherT putStrLn pure $ do
  pool <- getPool
  http <- HTTP.require
  ignore $ HTTP.listen' {e = NodeError} $
      decodeUri' (text "URI decode has failed" >=> status BAD_REQUEST)
      :> parseUrl' (const $ text "URL has invalid format" >=> status BAD_REQUEST)
      :> routes' (text "Resource could not be found" >=> status NOT_FOUND)
          [ get $ path "/query" $ \ctx =>
              text ctx.request.url.search ctx >>= status OK
          , get $ path "/parsed" $ Simple.search $ \ctx =>
              text (show ctx.request.url.search) ctx >>= status OK
          , get $ path "/db" :> \ctx => do
              putStrLn "querying db"
              let cs = getCountries pool
              x <- transform cs
              text (show x) ctx >>= status OK
          , get $ path "/foo" :> \ctx => do
              putStrLn "querying foo from db"
              let cs = getFoos pool
              x <- transform cs
              json (x) ctx >>= status OK
          , post
              $ TyTTP.URL.Path.path "/foo"
              $ consumes' [JSON]
                  (\ctx => text "Content cannot be parsed: \{ctx.request.body}" ctx >>= status BAD_REQUEST)
              $ \ctx => do
                let foo = ctx.request.body
                let q = insertFoo pool foo
                _ <- transform q
                text (show ctx.request.body) ctx >>= status OK
          , get $ path "/request" :> \ctx => do
              putStrLn "Calling http"
              res <- MkPromise $ \cb =>
                ignore $ http.get "http://localhost:3000/parsed?q=from-request" cb.onSucceded
              if res.statusCode == 200
                then
                  pure $
                    { response.status := OK
                    , response.headers := [("Content-Type", "text/plain")]
                    , response.body := MkPublisher $ \s => do
                        onData res s.onNext
                        onEnd res s.onSucceded
                        onError res s.onFailed
                    } ctx
                else
                  text "HTTP call failed with status code \{show res.statusCode}" ctx >>= status INTERNAL_SERVER_ERROR
          ]
