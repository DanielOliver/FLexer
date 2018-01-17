module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Helpers.React
open Fable.Helpers.React.Props

let root model dispatch =
    p []
      [ str "hello world"  ]


type Page =
  | Home

let toHash page =
  match page with
  | Home -> "#home"


let pageParser: Parser<Page->Page,Page> =
  oneOf [
    Elmish.Browser.UrlParser.map Home (Elmish.Browser.UrlParser.s "home")
  ]

let urlUpdate (result: Option<Page>) model =
  match result with
  | None ->
    console.error("Error parsing url")
    model,Navigation.modifyUrl (toHash Page.Home)
  | Some page ->
      5, []

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram (fun _ -> 0, Cmd.Empty) (fun a _ -> a, Cmd.Empty) root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
