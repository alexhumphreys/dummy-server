
import Rhone.JS
import Data.List1
import Data.String
import Data.MSF.Switch
import Text.CSS

%default total

-- wait n milliseconds before running the given action
%foreign """
browser:lambda:(h,w,x,y)=>{
  fetch('https://jsonplaceholder.typicode.com/todos/1')
    .then(response => response.json())
    .then(json => {
      console.log('h: ' + h);
      console.log('w: ' + w);
      console.log( + x);
      console.log('y: ' + y);
      console.log('json: ' + json);
      h([JSON.stringify(json)]);
    })
}
"""
prim__fetch : (x -> JSIO ()) -> PrimIO ()

fetch : HasIO io => (x -> JSIO ()) -> io ()
fetch run = primIO $ prim__fetch (run)

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
data Ev = Ajax (List1 String) | Click

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

-- We use an event switch to properly start our application
-- once the data from the ajax call has been loaded.
--
-- `switchE` switches only once, when its first argument
-- fires a `Left` event. In our case, this will be the
-- `List1` returned from the ajax call.
-- In case of a button click, we just inform our users, that
-- the data has not been loaded yet. We could also just disable
-- the button and enable it, once we got the ajax data.
msf : MSF M Ev ()
msf = switchE (arr waitForAjax) doCycle >>> innerHtml out

  where waitForAjax : Ev -> Either (List1 String) String
        waitForAjax Click     = Right "No data loaded yet!"
        waitForAjax (Ajax ss) = Left ss

        -- We cycle through the strings we got from the
        -- ajax call. This will be the route the event network
        -- takes as soon as we recieved the ajax call. We therefore
        -- delay this by one even, printing an info that the
        -- data has been loaded, before starting the cycling.
        doCycle : List1 String -> MSF M Ev String
        doCycle (h ::: t) =
          cycle (h :: t) >>> iPre "Data loaded!"

ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt contentDiv content
  h <- handler <$> env

  -- this simulates an ajax call. It will fire 5 seconds after
  -- the UI has been setup.
  --
  -- It is also possible to setup such delayed computations
  -- as part of an event sink (data output). This is what I did
  -- in the first version of the example.
  fetch (\foobar => h foobar)
  -- setTimeout 5000 (h . Ajax $ "Hello" ::: ["World"])
  pure(msf, pure ())

main : IO ()
main = runJS . ignore $ reactimateDom "somePrefix" ui
