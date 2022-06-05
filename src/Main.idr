module Main

import Data.Buffer
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

%language ElabReflection

record Example where
  constructor MkExample
  field : String
  opt : Maybe Int

%runElab derive "Example" [Generic, Meta, Show, Eq, RecordFromJSON]

data Country =
  MkCountry String Nat
Show Country where
  show (MkCountry x k) = "MkCountry \{x} \{show k}"

countryFromRow : (us : List Universe) -> (RowU us) -> Maybe Country
countryFromRow ([Str, Num]) ([v, x]) = Just $ MkCountry v (cast x)
countryFromRow (x :: _) _ = Nothing
countryFromRow ([]) _ = Nothing

tryCountry : Maybe (us ** Table us) -> Maybe (List Country)
tryCountry Nothing = Nothing
tryCountry (Just (MkDPair fst snd)) = traverse (countryFromRow fst) snd

fooToQuery : Example -> String

insertFoo : Pool -> Example -> PG.Promise.Promise String IO ()
insertFoo pool foo = do
  -- BAD: vulnerable to SQL injection
  -- need to work out how to pass a HList to the FFI
  ignore $ query pool "INSERT INTO foo(bar,baz) VALUES (6,8),(1,7);"

getCountries : Pool -> PG.Promise.Promise String IO (List Country)
getCountries pool = do
  b <- query pool "SELECT country,total FROM board"
  countries <- lift $ getAll b
  ls <- lift $ tryCountry countries
  case ls of
       Nothing => reject "Error: got Nothing"
       (Just cs) => pure $ trace (show cs) cs

resolve' : Core.Promise.Promise e m a -> (a -> m ()) -> (e -> m ()) -> m ()
resolve' (MkPromise cmd) ok err = do
  -- cmd ok err
  ?nnn

mapErr : (e -> e') -> Core.Promise.Promise e m a -> Core.Promise.Promise e' m a
mapErr f x = MkPromise $ \cb => do
  resolve' x
    (\a => cb.onSucceded a)
    (\e => cb.onFailed $ f e)

transform : PG.Promise.Promise e m a -> Core.Promise.Promise e m a
transform x = MkPromise $ \cb => do
  resolve x
    (\a => cb.onSucceded a)
    (\e => cb.onFailed e)

%foreign """
node:lambda: () => {
  const { Pool, Client } = require('pg')
  // pools will use environment variables
  // for connection information
  const pool = new Pool({
    user: 'postgres',
    host: '127.0.0.1',
    database: 'foo',
    password: 'mysecretpassword',
    port: 5432,
  })
  return pool
}
"""
prim__get_pool' : PrimIO Pool

-- for querying
export
getPool' : HasIO io => io Pool
getPool' = primIO $ prim__get_pool'

main : IO ()
main = eitherT putStrLn pure $ do
  pool <- getPool'
  http <- HTTP.require
  ignore $ HTTP.listen' {e = String} $
      decodeUri' (text "URI decode has failed" >=> status BAD_REQUEST)
      :> parseUrl' (const $ text "URL has invalid format" >=> status BAD_REQUEST)
      :> routes' (text "Resource could not be found" >=> status NOT_FOUND)
          [ post
            $ TyTTP.URL.Path.path "/json" $ ?kka
          , get $ path "/query" $ \ctx =>
              text ctx.request.url.search ctx >>= status OK
          , get $ path "/parsed" $ Simple.search $ \ctx =>
              text (show ctx.request.url.search) ctx >>= status OK
          , get $ path "/db" :> \ctx => do
              putStrLn "querying db"
              let cs = getCountries pool
              x <- transform cs
              text (show x) ctx >>= status OK
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
