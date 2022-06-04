module Main

import Data.Buffer
import Control.Monad.Either
import Control.Monad.Maybe
import Control.Monad.Trans
import Node.HTTP.Server
import Node.HTTP.Client
import Node
import System.Directory
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.Static
import TyTTP.HTTP
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path

%foreign """
node:lambda: () => {
  const { Pool, Client } = require('pg');
  const pool = new Pool();
  pool.query('SELECT NOW()', (err, res) => {
    console.log(err, res)
    pool.end()
  })
}
"""
ffi_require_pg : PrimIO ()

requirePg : IO ()
requirePg = primIO ffi_require_pg

export
data HTTPS : Type where [external]

%foreign "node:lambda: () => require('https')"
ffi_require : () -> PrimIO HTTPS

export
requireHTTPS : HasIO io => io HTTPS
requireHTTPS = primIO $ ffi_require ()

%foreign "node:lambda: (https, url, cb) => https.get(url, (res) => { cb(res)() })"
ffi_gethttps : HTTPS -> String -> (Node.HTTP.Client.IncomingMessage -> PrimIO ()) -> PrimIO ClientRequest

export
getHttps : HasIO io => HTTPS -> String -> (Node.HTTP.Client.IncomingMessage -> IO ()) -> io ClientRequest
getHttps https url cb = primIO $ ffi_gethttps https url $ \res => toPrim $ cb res

extraReq :
  Error e
  => HasIO io
  => Context me u v h1 s StringHeaders a b
  -> io $ Context me u v h1 Status StringHeaders a (Publisher IO e Buffer)
extraReq ctx = do
  putStrLn "extraReq"
  https <- requireHTTPS
  x <- getHttps https "https://jsonplaceholder.typicode.com/todos/1" $ \res => do
        putStrLn $ show res.statusCode
        onData res putStrLn
        -- TODO somehow use this when responding to calls to /extraReq
  text "extraReq" ctx >>= status OK

doStuff :
  Error e
  => HasIO io
  => Context me u v h1 s StringHeaders a b
  -> io $ Context me u v h1 Status StringHeaders a (Publisher IO e Buffer)
doStuff ctx = do
  putStrLn "doStuff"
  text "foo" ctx >>= status OK

sendError :
  Error e
  => HasIO io
  => Status
  -> String
  -> Context me u v h1 s StringHeaders a b
  -> io $ Context me u v h1 Status StringHeaders a (Publisher IO e Buffer)
sendError st str ctx = do
  putStrLn "hello"
  text str ctx >>= status st

routeDef : Error e
  => HasIO io
  => String
  -> StaticRequest e String
  -> Promise e io $ StaticResponse e String
routeDef folder =
    let routingError = sendError NOT_FOUND "Resource could not be found"
        urlError = \err => sendError BAD_REQUEST "URL has invalid format"
    in
      parseUrl' urlError :>
        routes' routingError
          [ get $ TyTTP.URL.Path.path "/query" $ \ctx =>
                doStuff ctx
          , get $ TyTTP.URL.Path.path "/extraReq" $ \ctx =>
                extraReq ctx
          ]

main : IO ()
main = do
  requirePg
  eitherT putStrLn pure $ do
  Just folder <- currentDir
    | _ => putStrLn "There is no current folder"

  http <- HTTP.require
  https <- requireHTTPS
  -- defer $ ignore $ getHttps https "https://jsonplaceholder.typicode.com/todos/1" $ \res => do
        -- putStrLn res.statusCode
        -- onData res putStrLn

  ignore $ HTTP.listen' $ routeDef {io=IO} "\{folder}/"

  -- defer $ ignore $ http.get "http://localhost:3000/query" $ \res => do
      -- putStrLn res.statusCode
      -- onData res putStrLn
      -- server.close

