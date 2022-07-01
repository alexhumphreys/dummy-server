import Rhone.JS
import Data.List1
import Data.String
import Data.MSF.Switch
import Text.CSS
import Generics.Derive
import JSON

%default total

{-
{
	"0": {
		"userId": 1,
		"id": 1,
		"title": "delectus aut autem",
		"completed": false
	}
}
-}

%language ElabReflection

record Todo where
  constructor MkTodo
  userId : Nat
  id : Nat
  title : String
  completed : Bool

%runElab derive "Todo" [Generic, Meta, Show, Eq, RecordToJSON, RecordFromJSON]

-- I set a timeout once the request has been received to make
-- it clearer how the UI behaves until then.
%foreign """
browser:lambda:(url,h,w,x,y)=>{
  fetch(url)
    .then(response => response.json())
    .then(json => {
      setTimeout(() => {h(JSON.stringify(json))(w);}, 5000);
    })
}
"""
prim__fetch : String -> (String -> IO ()) -> PrimIO ()

fetch : HasIO io => String -> (String -> JSIO ()) -> io ()
fetch url run = primIO $ prim__fetch url (runJS . run)

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

listTodoDiv : ElemRef HTMLDivElement
listTodoDiv = Id Div "somePrefix_listTodo"

selectedTodoDiv : ElemRef HTMLDivElement
selectedTodoDiv = Id Div "somePrefix_selectedTodo"

btn : ElemRef HTMLButtonElement
btn = Id Button "my_button"

||| The type of events our UI fires.
||| This is either some data we get back from an ajax call
||| or the click of a button.
|||
||| In addition, we define an `Init` event, which is fired after
||| the UI has been setup. This will start the ajax request.
data Ev = ListTodo (List Todo) | SelectedTodo (Nat) | Click | Init | Err (String)

selectedTodo : Todo -> Node Ev
selectedTodo x =
  let todoId = Main.Todo.id x in
    div [] [Text $ show $ todoId, Text $ title x, Text "do GET `/posts/\{show todoId}/comments` here"]

todoItemRef : Nat -> ElemRef Div
todoItemRef n = Id Div "todoItem\{show n}"

todoItem : Todo -> Node Ev
todoItem x =
  let todoId = Main.Todo.id x in
  div [ref $ todoItemRef todoId, onClick $ SelectedTodo todoId] [Text $ show $ todoId, Text $ title x]

listTodos : List Todo -> Node Ev
listTodos xs =
  div [] $ map todoItem xs

content : Node Ev
content =
  div []
    [ div [] ["content2"]
    , div [ref out] []
    , div [ref listTodoDiv] []
    , div [ref selectedTodoDiv] []
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

go : Either String (List Todo) -> MSF M String ()
go (Right x) = const (show x) >>> innerHtml listTodoDiv
go (Left x) = const x >>> innerHtml out

msf : (String -> JSIO ()) -> MSF M Ev ()
msf get = switchE (arrM waitForAjax) doCycle >>> innerHtml out

  where waitForAjax : Ev -> M (Either (List Todo) String)
        waitForAjax Click     = pure $ Right "No data loaded yet!"
        waitForAjax Init      = fetch "https://jsonplaceholder.typicode.com/todos" get $> Right "Request sent."
        waitForAjax (ListTodo ss) = pure $ Left ss
        waitForAjax (Err st) = pure $ Right st
        waitForAjax (SelectedTodo x) = fetch "https://jsonplaceholder.typicode.com/todos/\{show x}" get $> Right "Requesting Todo \{show x}"

        doCycle : List Todo -> MSF M Ev String
        doCycle ts = const (show ts) >>> iPre "Data loaded!"

parseResponse : String -> Ev
parseResponse str =
  case decode {a=List Todo} str of
       (Left x) => Err "failed to parse json as Todo: \{str}"
       (Right x) => ListTodo x

ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt contentDiv content
  h <- handler <$> env

  pure(msf $ \s => h (parseResponse s), pure ())

main : IO ()
main = runJS . ignore $ reactimateDomIni Init "somePrefix" ui
