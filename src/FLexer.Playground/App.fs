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
    | Types.Msg.BasicExampleStringFormat ->
        Types.Model.ExampleStringFormat, Cmd.Empty

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

    | Types.Msg.ExampleStringFormat stringFormatText ->
        {
            ParseResult = Types.ParseResult.StringFormatParse (FLexer.Example.Core.StringFormat.ExampleTester <| stringFormatText.Trim())
            CurrentText = stringFormatText
            CurrentPage = Types.Page.ExampleStringFormat
        }, Cmd.Empty


let createTokenTableFromRows rows =
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

let createTextTableFromRows rows =
    [   R.thead []
            [   R.tr []
                    [   R.th [] [ str "Text" ]
                    ]
            ]
        R.tbody [] rows
    ]
    |>
    R.table
        [   ClassName "table is-bordered is-narrow is-hoverable is-fullwidth"
        ]

let isWhitespace = System.String.IsNullOrWhiteSpace
let getText text = if isWhitespace text then "(whitespace)" else text
let italicizeIfWhitespace text =
    R.str (getText text)
    |> (if isWhitespace text then List.singleton >> (R.i []) else id)

let getTokenTable (status: FLexer.Core.ClassifierStatus<_>) =
    status.Consumed
    |> List.rev
    |> List.mapi(fun index t ->
        R.tr [ Id <| index.ToString() ]
            [   R.td [] [ str <| t.StartCharacter.ToString() ]
                R.td [] [ str <| t.EndCharacter.ToString() ]
                R.td [] [ italicizeIfWhitespace t.Text ]
                R.td [] [ str <| t.Classification.ToString() ]
            ]
    )
    |> createTokenTableFromRows

let getTextTable (status: FLexer.Core.ClassifierStatus<_>) =
    status.ConsumedWords
    |> List.rev
    |> List.mapi(fun index t ->
        R.tr [ Id <| index.ToString() ]
             [   R.td [] [ italicizeIfWhitespace t ]
             ]
    )
    |> createTextTableFromRows

let getParseResultWithTextArea (model: Types.Model) textAreaWithUpdate =
    match model.ParseResult with
    | Types.ParseResult.JSONParse result ->
        match result with
        | Ok(jsonResult, status) -> true, getTokenTable status, getTextTable status
        | Error(error) -> false, getTokenTable error.LastStatus, getTextTable error.LastStatus
    | Types.ParseResult.SQLParse result ->
        match result with
        | Ok(sqlResult, status) -> true, getTokenTable status, getTextTable status
        | Error(error) -> false, getTokenTable error.LastStatus, getTextTable error.LastStatus
    | Types.ParseResult.StringFormatParse result ->
        match result with
        | Ok(parseResult, status) -> true, getTokenTable status, getTextTable status
        | Error(error) -> false, getTokenTable error.LastStatus, getTextTable error.LastStatus

    |> (fun (isSuccess, tokenTable, textTable) ->
        R.div [ ClassName "columns is-desktop" ]
            [
                R.div [ ClassName "column" ]
                    [
                        R.h1 [ ClassName "title" ] [ str (if isSuccess then "Success" else "Incomplete") ]
                        textAreaWithUpdate
                        R.h3 [ ClassName "subtitle" ] [ str "Tokens" ]
                        tokenTable
                    ]
                R.div [ ClassName "column is-narrow" ]
                    [
                        R.h3 [ ClassName "subtitle" ] [ str "ConsumedText" ]
                        textTable
                    ]
            ]
    )


let root (model: Types.Model) dispatch =
    printfn "parseModel %A" model.ParseResult
    let dispatchFunc text =
        printfn "dispatch %s" text
        match model.CurrentPage with
        | Types.Page.ExampleJSON -> text |> Types.Msg.ExampleJSON |> dispatch
        | Types.Page.ExampleSQL -> text |> Types.Msg.ExampleSQL |> dispatch
        | Types.Page.ExampleStringFormat -> text |> Types.Msg.ExampleStringFormat |> dispatch

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
                ClassName "textarea is-focused is-medium"
            ] []
        |> List.singleton
        |> R.div [ ClassName "control" ]
        |> List.singleton
        |> R.div [ ClassName "field" ]

    let editor = 
        getParseResultWithTextArea model textUpdateArea
        |> List.singleton
        |> R.div [ ClassName "container content is-fluid" ]
        |> List.singleton
        |> R.section [ ClassName "section" ]


    R.div []
        [   R.nav [ ClassName "navbar is-info"; Role "navigation" ]
                [   R.div [ ClassName "navbar-menu" ]
                        [   R.div [ ClassName "navbar-start" ]
                                [   R.a [ ClassName "navbar-item"; Href "#examplesql" ] [ R.str "SQL Select"  ]
                                    R.a [ ClassName "navbar-item"; Href "#examplestringformat" ] [ R.str "String Format"  ]
                                    R.a [ ClassName "navbar-item"; Href "#examplejson" ] [ R.str "JSON"  ]
                                ]
                        ]
                ]
            editor
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
