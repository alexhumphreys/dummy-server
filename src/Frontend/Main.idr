import Rhone.JS
import Data.String
import Text.CSS

%default total

public export
MSel : Type -> Type
MSel = DomIO () JSIO

msf : MSF MSel () ()
msf = Const ()

appStyle : ElemRef HTMLStyleElement
appStyle = Id Style "appstyle"

public export
contentDiv : ElemRef HTMLBodyElement
contentDiv = Id Body "content"

content : Node ()
content =
  div []
      [ div [] ["rhone-js: Examples"]
      , div []
          [ label [] ["Choose an Example"]
          , select
              [ classes []]
              [ option [ value "reset", selected True ] ["Counting Clicks"]
              , option [ value "performance" ] ["Performance"]
              , option [ value "fractals" ] ["Fractals"]
              , option [ value "balls" ] ["Bouncing Balls"]
              , option [ value "math" ] ["Math Game"]
              ]
          ]
      ]

coreCSS : List (Rule 1)
coreCSS =
  [ elem Html !!
      [ Height          .= perc 100]
    ]

allRules : String
allRules = fastUnlines . map Text.CSS.Render.render
         $ coreCSS

ui : MSel (MSF MSel () (), JSIO ())
ui = do
  rawInnerHtmlAt appStyle allRules
  innerHtmlAt contentDiv content
  pure (msf, pure ())

main : IO ()
main = runJS . ignore $ reactimateDomIni () "somePrefix" ui
