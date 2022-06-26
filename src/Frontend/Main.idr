import Rhone.JS
import Data.String
import Data.MSF.Switch
import Text.CSS

%default total

%foreign "browser:lambda:(n,h,w)=>setTimeout(() => h(w),n)"
prim__setTimeout : Bits32 -> IO () -> PrimIO ()

setTimeout' : HasIO io => Bits32 -> JSIO () -> io ()
setTimeout' millis run = primIO $ prim__setTimeout 1000 (runJS run)

setTimeout : HasIO io => JSIO () -> io ()
setTimeout run = setTimeout' 1000 run

appStyle : ElemRef HTMLStyleElement
appStyle = Id Style "appstyle"

public export
contentDiv : ElemRef HTMLBodyElement
contentDiv = Id Body "content"

out : ElemRef HTMLDivElement
out = Id Div "somePrefix_out"

content : Node (List String)
content =
  div []
    [ div [] ["content2"]
    , div [ref out] []
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
M = DomIO (List String) JSIO

msf : (timer : JSIO ()) -> MSF M (List String) ()
msf timer = drswitchWhen neutral one two
  where
    MSFEvent : Type -> Type
    MSFEvent = Data.MSF.Event.Event

    one : MSF M (List String) (MSFEvent (List String))
    one = arrM ?someTimerFunction -- (liftIO . timer)

    two : List String -> MSF M (List String) ()
    two ls = case ls of
                  [] => cycle ["empty"] >>> innerHtml out
                  ls1@(x :: xs) => cycle ls1 >>> innerHtml out

ui : M (MSF M (List String) (), JSIO ())
ui = do
  innerHtmlAt contentDiv content
  h <- handler <$> env
  let timer : JSIO ()
      timer = do
        setTimeout $ h $ ?someListStr -- ["foo", "bar"]
  pure(msf timer, pure ())

main : IO ()
main = runJS . ignore $ reactimateDom "somePrefix" ui
