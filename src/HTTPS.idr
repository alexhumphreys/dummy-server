module HTTPS

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

