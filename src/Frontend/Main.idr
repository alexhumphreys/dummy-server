import Rhone.JS
import Data.String
import Data.MSF.Switch
import Text.CSS
import Data.List1

%default total

%foreign "browser:lambda:(n,h,w)=>setTimeout(() => h(w),n)"
prim__setTimeout : Bits32 -> IO () -> PrimIO ()

setTimeout' : HasIO io => Bits32 -> JSIO () -> io ()
setTimeout' millis run = primIO $ prim__setTimeout 1000 (runJS run)

setTimeout : HasIO io => JSIO () -> io ()
setTimeout run = setTimeout' 1000 run

public export
MSel : Type -> Type
MSel = DomIO () JSIO

msf : MSF MSel () ()
msf = Const ()

M : Type -> Type
M = DomIO (List String) JSIO

appStyle : ElemRef HTMLStyleElement
appStyle = Id Style "appstyle"

public export
contentDiv : ElemRef HTMLBodyElement
contentDiv = Id Body "content"

out : ElemRef HTMLDivElement
out = Id Div "somePrefix_out"

msf2 : (timer : JSIO ()) -> MSF M (List String) ()
msf2 timer = drswitchWhen neutral one two
  where
    MSFEvent : Type -> Type
    MSFEvent = Data.MSF.Event.Event

    one : MSF M (List String) (MSFEvent (List String))
    one = arrM ?someTimerFunction -- (liftIO . timer)

    two : List String -> MSF M (List String) ()
    two ls = case ls of
                  [] => cycle ["empty"] >>> innerHtml out
                  ls1@(x :: xs) => cycle ls1 >>> innerHtml out

bar : Node ()
bar = div [] [div [] ["bar"]]

content : Node ()
content =
  div []
    [ div [] ["content"]
    , div [ref out] []
    ]

content2 : Node (List String)
content2 =
  div []
    [ div [] ["content2"]
    , div [ref out] []
    ]

bar2 : Node (List String)
bar2 = div [] [div [] ["bar2"]]

coreCSS : List (Rule 1)
coreCSS =
  [ elem Html !!
      [ Height          .= perc 100]
    ]

allRules : String
allRules = fastUnlines . map Text.CSS.Render.render
         $ coreCSS

ui2 : M (MSF M (List String) (), JSIO ())
ui2 = do
  -- rawInnerHtmlAt appStyle allRules
  innerHtmlAt contentDiv content2
  hi <- handler <$> env
  let timer : JSIO ()
      timer = do
        x <- setTimeout (hi ?someList) --["foo", "bar", "baz"])
        pure ()
  pure (msf2 timer, pure ())

ui : MSel (MSF MSel () (), JSIO ())
ui = do
  rawInnerHtmlAt appStyle allRules
  innerHtmlAt contentDiv content
  innerHtmlAt out bar
  pure (msf, pure ())

main : IO ()
main = runJS . ignore $ reactimateDomIni () "somePrefix" ui
