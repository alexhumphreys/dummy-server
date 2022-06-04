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
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search

import Postgres
import Promise
import Util

%foreign (promisifyPrim "(_,err)=>new Promise((resolve,reject)=>reject(err))")
reject__prim : String -> promise a

export
reject : String -> Promise a
reject err =
  promisify (reject__prim err)

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

getCountries : Pool -> Promise (List Country)
getCountries pool = do
  b <- query pool "SELECT country,total FROM board"
  countries <- lift $ getAll b
  ls <- lift $ tryCountry countries
  case ls of
       Nothing => reject "Error: got Nothing"
       (Just cs) => pure cs

-- doQuery : HasIO io => Pool -> String -> (IncomingMessage -> IO ()) -> io ClientRequest

transform : Promise.Promise a -> (Callbacks String IO a -> ())

returnWithDB : Context Method (URL String Path String) Version (List (String, String)) Status (List (String, String)) (Publisher IO NodeError Buffer) ()
  -> Promise (List Country)
  -> Lazy (Promise String IO (Context Method (URL String Path String) Version (List (String, String)) Status (List (String, String)) (Publisher IO NodeError Buffer) (Publisher IO NodeError Buffer)))
returnWithDB ctx@(MkContext request response) prom = do
  putStrLn "hello"
  ?returnWithDB_rhs_0
  pure $
    { response.status := OK
    , response.headers := [("Content-Type", "text/plain")]
    , response.body := MkPublisher $ \s => do
        let x = MkPromise $ \cb =>
                case cb of
                     (MkCallbacks onSucceded onFailed) => ?kj_0 -- MkCallbacks (\a => ?foo) (\e => ?bar)
        pure ()
    } ctx

main : IO ()
main = do
  pool <- getPool
  http <- HTTP.require
  ignore $ HTTP.listen' {e = String} $
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
              -- b <- query pool "SELECT country,total FROM board"
              returnWithDB ctx cs
          , get $ path "/request" :> \ctx => do
              putStrLn "Calling http"
              res <- MkPromise $ -- \cb =>
                                       ?kjkaa
                -- ignore $ http.get "http://localhost:3000/parsed?q=from-request" cb.onSucceded
              ?kjk
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
