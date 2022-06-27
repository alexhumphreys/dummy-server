import Rhone.JS
import Data.List1
import Data.String
import Data.MSF.Switch
import Text.CSS

%default total

-- I set a timeout once the request has been received to make
-- it clearer how the UI behaves until then.
%foreign """
browser:lambda:(h,w,x,y)=>{
  fetch('https://jsonplaceholder.typicode.com/todos/1')
    .then(response => response.json())
    .then(json => {
      setTimeout(() => {h(JSON.stringify(json))(w);}, 5000);
    })
}
"""
prim__fetch : (String -> IO ()) -> PrimIO ()

fetch : HasIO io => (String -> JSIO ()) -> io ()
fetch run = primIO $ prim__fetch (runJS . run)

-- wait n milliseconds before running the given action
%foreign "browser:lambda:(n,h,w)=>setTimeout(() => h(w),n)"
prim__setTimeout : Bits32 -> IO () -> PrimIO ()

setTimeout' : HasIO io => Bits32 -> JSIO () -> io ()
setTimeout' millis run = primIO $ prim__setTimeout millis (runJS run)

-- wait n milliseconds before running the given action
setTimeout : HasIO io => Nat -> JSIO () -> io ()
setTimeout n run = setTimeout' (cast n) run

appStyle : ElemRef HTMLStyleElement
appStyle = Id Style "appstyle"

public export
contentDiv : ElemRef HTMLBodyElement
contentDiv = Id Body "content"

out : ElemRef HTMLDivElement
out = Id Div "somePrefix_out"

btn : ElemRef HTMLButtonElement
btn = Id Button "my_button"

||| The type of events our UI fires.
||| This is either some data we get back from an ajax call
||| or the click of a button.
|||
||| In addition, we define an `Init` event, which is fired after
||| the UI has been setup. This will start the ajax request.
data Ev = Ajax (List1 String) | Click | Init

content : Node Ev
content =
  div []
    [ div [] ["content2"]
    , div [ref out] []
    , button [ ref btn, onClick Click] [ "Click me!" ]
    ]

coreCSS : List (Rule 1)
coreCSS =
  [ elem Html !!
      [ Height .= perc 100]
    ]

allRules : String
allRules = fastUnlines . map Text.CSS.Render.render
         $ coreCSS

M : Type -> Type
M = DomIO Ev JSIO

msf : (String -> JSIO ()) -> MSF M Ev ()
msf get = switchE (arrM waitForAjax) doCycle >>> innerHtml out

  where waitForAjax : Ev -> M (Either (List1 String) String)
        waitForAjax Click     = pure $ Right "No data loaded yet!"
        waitForAjax Init      = fetch get $> Right "Request sent."
        waitForAjax (Ajax ss) = pure $ Left ss

        doCycle : List1 String -> MSF M Ev String
        doCycle (h ::: t) =
          cycle (h :: t) >>> iPre "Data loaded!"

ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt contentDiv content
  h <- handler <$> env

  pure(msf $ \s => h (Ajax $ split (',' ==) s), pure ())

main : IO ()
main = runJS . ignore $ reactimateDomIni Init "somePrefix" ui
