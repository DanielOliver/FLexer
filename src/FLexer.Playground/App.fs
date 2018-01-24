module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Helpers.React
open App.State

let root model dispatch =
    p []
      [ str "hello world"  ]


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
