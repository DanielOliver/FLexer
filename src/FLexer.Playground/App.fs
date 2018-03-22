module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
module R = Fable.Helpers.React
open App.State
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop


let init result =
    let (model, cmd) = urlUpdate result Types.Model.ExampleSQL
    printfn "Init %A" model
    model, cmd

let update (msg:Types.Msg) model =
    match msg with
    | Types.Msg.BasicExampleJSON ->
        Types.Model.ExampleJSON, Cmd.Empty
    | Types.Msg.BasicExampleSQL ->
        Types.Model.ExampleSQL, Cmd.Empty
    | Types.Msg.ExampleJSON jsonText ->
        {
            ParseResult = Types.ParseResult.JSONParse (FLexer.Example.Core.JSON.ExampleTester jsonText)
            CurrentText = jsonText
            CurrentPage = Types.Page.ExampleJSON
        }, Cmd.Empty
    | Types.Msg.ExampleSQL sqlText ->
        {
            ParseResult = Types.ParseResult.SQLParse (FLexer.Example.Core.BasicSQL.ExampleTester sqlText)
            CurrentText = sqlText
            CurrentPage = Types.Page.ExampleSQL
        }, Cmd.Empty

let root (model: Types.Model) dispatch =
    printfn "parseModel %A" model.ParseResult
    let dispatchFunc text =
        printfn "dispatch %s" text
        match model.CurrentPage with
        | Types.Page.ExampleJSON -> text |> Types.Msg.ExampleJSON |> dispatch
        | Types.Page.ExampleSQL -> text |> Types.Msg.ExampleSQL |> dispatch

    R.input
        [   ClassName "input"
            Type "text"
            Multiple true
            Placeholder ""
            Value model.CurrentText
            AutoFocus true
            OnChange (fun ev -> !!ev.target?value |> dispatchFunc )
        ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
