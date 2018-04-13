module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
module R = Fable.Helpers.React
open App.State
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop


importAll "../../sass/main.sass"

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
            ParseResult = Types.ParseResult.JSONParse (FLexer.Example.Core.JSON.ExampleTester <| jsonText.Trim())
            CurrentText = jsonText
            CurrentPage = Types.Page.ExampleJSON
        }, Cmd.Empty
    | Types.Msg.ExampleSQL sqlText ->
        {
            ParseResult = Types.ParseResult.SQLParse (FLexer.Example.Core.BasicSQL.ExampleTester <| sqlText.Trim())
            CurrentText = sqlText
            CurrentPage = Types.Page.ExampleSQL
        }, Cmd.Empty

let createParseResultSuccessTable rows =
    [   R.thead []
            [   R.tr []
                    [   R.th [] [ str "StartChar" ]
                        R.th [] [ str "EndChar" ]
                        R.th [] [ str "Text" ]
                        R.th [] [ str "Classification" ]
                    ]
            ]
        R.tbody [] rows
    ]
    |>
    R.table
        [   ClassName "table is-bordered is-narrow is-hoverable is-fullwidth"
        ]

let getParseResult (model: Types.Model) =
    let getSuccessTable (status: FLexer.Core.ClassifierStatus<_>) =
        let getText text = if System.String.IsNullOrWhiteSpace(text) then "(whitespace)" else text

        status.Consumed
        |> List.rev
        |> List.mapi(fun index t ->
            R.tr [ Id <| index.ToString() ]
                [   R.td [] [ str <| t.StartCharacter.ToString() ]
                    R.td [] [ str <| t.EndCharacter.ToString() ]
                    R.td [] [ str <| getText t.Text ]
                    R.td [] [ str <| t.Classification.ToString() ]
                ]
        )
        |> createParseResultSuccessTable
        
    match model.ParseResult with
    | Types.ParseResult.JSONParse result ->
        match result with
        | Ok(jsonResult, status) -> true, getSuccessTable status
        | Error(error) -> false, getSuccessTable error.LastStatus
    | Types.ParseResult.SQLParse result ->
        match result with
        | Ok(sqlResult, status) -> true, getSuccessTable status
        | Error(error) -> false, getSuccessTable error.LastStatus
    |> (fun (isSuccess, table) ->
        R.div []
            [   R.h2 [ ClassName "title" ] [ str (if isSuccess then "Success" else "Incomplete") ]
                table
            ]
    )


let root (model: Types.Model) dispatch =
    printfn "parseModel %A" model.ParseResult
    let dispatchFunc text =
        printfn "dispatch %s" text
        match model.CurrentPage with
        | Types.Page.ExampleJSON -> text |> Types.Msg.ExampleJSON |> dispatch
        | Types.Page.ExampleSQL -> text |> Types.Msg.ExampleSQL |> dispatch

    let textUpdateArea =
        R.textarea
            [   Rows 7.0
                Cols 80.0
                Multiple true
                Placeholder ""
                Type "text"
                Value model.CurrentText
                AutoFocus true
                OnChange (fun ev -> !!ev.target?value |> dispatchFunc )
                ClassName "textarea is-focused"
            ] []
        |> List.singleton
        |> R.div [ ClassName "control" ]
        |> List.singleton
        |> R.div [ ClassName "field" ]

    R.div []
        [   textUpdateArea
            getParseResult model
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
