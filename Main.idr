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
          ]

main : IO ()
main = eitherT putStrLn pure $ do
  Just folder <- currentDir
    | _ => putStrLn "There is no current folder"

  http <- HTTP.require
  ignore $ HTTP.listen' $ routeDef {io=IO} "\{folder}/"

  -- defer $ ignore $ http.get "http://localhost:3000/query" $ \res => do
      -- putStrLn res.statusCode
      -- onData res putStrLn
      -- server.close

