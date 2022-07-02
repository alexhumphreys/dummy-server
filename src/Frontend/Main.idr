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

record FetchResponse where
  constructor MkFetchResponse
  url : Nat
  status : Nat
  statusText : String
  redirected : Bool
  ok : Bool

%runElab derive "FetchResponse" [Generic, Meta, Show, RecordToJSON, RecordFromJSON]

record Todo where
  constructor MkTodo
  userId : Nat
  id : Nat
  title : String
  completed : Bool

%runElab derive "Todo" [Generic, Meta, Show, Eq, RecordToJSON, RecordFromJSON]

-- I set a timeout once the request has been received to make
-- it clearer how the UI behaves until then.
-- TODO should pass an "on error" function here, then have a function print that output
%foreign """
browser:lambda:(url,h,w,e,y)=>{
  fetch(url)
    .then(response => {
       if (!response.ok) {
        w(JSON.stringify({
          url: response.url,
          status: response.status,
          statusText: response.statusText,
          redirected: response.redirected,
          ok: response.ok,
        }))(e);
        throw new Error('Network response was not OK');
       } else {
         return response.json();
       }
    })
    .then(json => {
      h(JSON.stringify(json))(e);
    })
    .catch(error => {
      w(error.message)(e)
      console.error(error)
    })
}
"""
prim__fetch : String -> (String -> IO ()) -> (String -> IO ()) -> PrimIO ()

fetch : HasIO io => String -> (String -> JSIO ()) -> (String -> JSIO ()) -> io ()
fetch url run err = primIO $ prim__fetch url (runJS . run) (runJS . err)

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

aPrefix : String
aPrefix = "somePrefix"

out : ElemRef HTMLDivElement
out = Id Div "\{aPrefix}_out"

errorDiv : ElemRef HTMLDivElement
errorDiv = Id Div "\{aPrefix}_errorDiv"

listTodoDiv : ElemRef HTMLDivElement
listTodoDiv = Id Div "\{aPrefix}_listTodo"

selectedTodoDiv : ElemRef HTMLDivElement
selectedTodoDiv = Id Div "\{aPrefix}_selectedTodo"

createTodoDiv : ElemRef HTMLDivElement
createTodoDiv = Id Div "\{aPrefix}_createTodo"

userDiv : ElemRef HTMLDivElement
userDiv = Id Div "\{aPrefix}_user"

btn : ElemRef HTMLButtonElement
btn = Id Button "my_button"

||| The type of events our UI fires.
||| This is either some data we get back from an ajax call
||| or the click of a button.
|||
||| In addition, we define an `Init` event, which is fired after
||| the UI has been setup. This will start the ajax request.

todoItemRef : Nat -> ElemRef Div
todoItemRef n = Id Div "todoItem\{show n}"

coreCSS : List (Rule 1)
coreCSS =
  [ elem Html !!
      [ Height .= perc 100]
    ]

allRules : String
allRules = fastUnlines . map Text.CSS.Render.render
         $ coreCSS

record User where
  constructor MkUser
  id   : Nat
  name : String
  username : String
  email : String

%runElab derive "User" [Generic, Meta, Show, Eq, RecordToJSON, RecordFromJSON]

data Ev' : Type where
  ||| Initial event
  Init'     : Ev'

  ||| An ajax call returned a list of todos
  ListLoaded : List Todo -> Ev'

  ||| An ajax call returned a list of todos
  SingleLoaded : Todo -> Ev'

  ||| An ajax call returned a user
  UserLoaded : User -> Ev'

  ||| A single item in the todo list was selected
  Selected' : Nat -> Ev'

  ||| A single item in the todo list was selected
  ClickAdd' : Ev'

  ||| Error
  Err' : String -> Ev'

%runElab derive "Ev'" [Generic]

todoItem' : Todo -> Node Ev'
todoItem' x =
  let todoId = Main.Todo.id x in
  div [ref $ todoItemRef todoId, onClick $ Selected' todoId] [Text $ show $ todoId, Text $ title x]

listTodos' : List Todo -> Node Ev'
listTodos' xs =
  div [] $ map todoItem' xs

M' : Type -> Type
M' = DomIO Ev' JSIO

parseResponse' : String -> Ev'
parseResponse' str =
  case decode {a=List Todo} str of
       (Left x) => Err' "failed to parse json as Todo: \{str}"
       (Right x) => ListLoaded x

fireEv' : Nat -> Ev' -> M' ()
fireEv' n ev = do
  h <- handler <$> env
  setTimeout n (h ev)
  pure ()

||| fire off random Ev'
fireEv : Ev' -> M' ()
fireEv ev = fireEv' 0 ev

fetchParseEvent : FromJSON a => String -> (a -> Ev') -> M' ()
fetchParseEvent url ev = do
    h <- handler <$> env
    fetch url (\s => h (parseType s)) (\e => h (handleError e))
    pure ()
where
  handleError : String -> Ev'
  handleError str =
    case decode {a=FetchResponse} str of
         (Left x) =>  Err' "Fetch err: \{str}"
         (Right x) => Err' "Fetch err: \{show x}"

  parseType : String -> Ev'
  parseType str =
    case decode {a=a} str of
         (Left x) => Err' "failed to parse json: \{str}"
         (Right x) => ev x

-- below, I define some dummy MSFs for handling each of the
-- events in question:
onInit : MSF M' (NP I []) ()
onInit = arrM $ \_ => do
  fetchParseEvent {a=List Todo} "https://jsonplaceholder.typicode.com/todos" ListLoaded
-- invoke `get` with the correct URL

-- prints the list to the UI.
-- this requires a call to `innerHtmlAt` to set up the necessary event handlers
onListLoaded : MSF M' (NP I [List Todo]) ()
onListLoaded = do
  arrM $ \[ts] => do
    innerHtmlAt listTodoDiv $ listTodos' $ take 21 ts

onUserLoaded : MSF M' (NP I [User]) ()
onUserLoaded = arrM $ (\[u] => innerHtmlAt userDiv $ renderUser u)
where
  renderUser : User -> Node Ev'
  renderUser u = div []
    [Text $ User.name u, Text $ username u, Text $ email u]

onSingleLoaded : MSF M' (NP I [Todo]) ()
onSingleLoaded = arrM $ (\[t] => do
  innerHtmlAt selectedTodoDiv $ selectedTodo' t
  fetchParseEvent {a=User} "https://jsonplaceholder.typicode.com/users/\{show $ Main.Todo.userId t}" UserLoaded)
where
  selectedTodo' : Todo -> Node Ev'
  selectedTodo' x =
    let todoId = Main.Todo.id x in
      div []
        [ Text $ show $ todoId, Text $ title x, Text "Selected!"
        , div [ref userDiv] []
        ]
-- onSingleLoaded = arrM $ \[t] => innerHtmlAt selectedTodoDiv ...

onSelected : MSF M' (NP I [Nat]) ()
onSelected = arrM $ \[n] => fetchParseEvent {a=Todo} "https://jsonplaceholder.typicode.com/todos/\{show n}" SingleLoaded
-- onSelected = arrM $ \[d] => -- invoke `get` with the correct URL

onErr : MSF M' (NP I [String]) ()
onErr = arrM $ \[s] => innerHtmlAt errorDiv $ renderErr' s
where
  renderErr' : String -> Node Ev'
  renderErr' x = div [] [Text x]
-- onErr = arrM $ \[s] => -- print error message to a UI element

{-
div [ class ballsContent ]
    [ lbl "Number of balls:" lblCount
    , input [ ref txtCount
            , onInput (const NumIn)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [1,1000]"
            ] []
    , button [ref btnRun, onClick Run, classes [widget,btn]] ["Run"]
    , div [ref log] []
    , canvas [ref out, width wcanvas, height wcanvas] []
    ]
    -}
onClickAdd : MSF M' (NP I []) ()
onClickAdd = arrM $ \_ => innerHtmlAt createTodoDiv renderForm
where
  btnCreate : ElemRef HTMLButtonElement
  btnCreate = Id Button "\{aPrefix}_createTodo"
  txtTitle : ElemRef HTMLInputElement
  txtTitle = Id Input "\{aPrefix}_newTitle"
  lbl : (text: String) -> (class : String) -> Node ev
  lbl txt cl = label [] [Text txt]

  renderForm : Node Ev'
  renderForm =
    div []
      [ lbl "Title:" ""
      , input [ ref txtTitle
              , placeholder "some title"
              ] []
      , button [ref btnCreate] ["Create Todo"]
      ]
-- invoke `get` with the correct URL

sf : MSF M' Ev' ()
sf = toI . unSOP . from ^>> collect [ onInit
                                    , onListLoaded
                                    , onSingleLoaded
                                    , onUserLoaded
                                    , onSelected
                                    , onClickAdd
                                    , onErr
                                    ]

content' : Node Ev'
content' =
  div []
    [ div [] ["content2"]
    , div [ref out] []
    , div [ref errorDiv] []
    , div [ref listTodoDiv] []
    , div [ref selectedTodoDiv] []
    , div [ref createTodoDiv] []
    , button [ ref btn, onClick ClickAdd'] [ "Add todo" ]
    ]

ui' : M' (MSF M' Ev' (), JSIO ())
ui' = do
  innerHtmlAt contentDiv content'

  pure(sf, pure ())

main : IO ()
main = runJS . ignore $ reactimateDomIni Init' aPrefix ui'
