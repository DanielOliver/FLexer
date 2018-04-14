module App.Types

open FLexer.Core
open FLexer.Example.Core.JSON
open FLexer.Example.Core.BasicSQL

type Page =
    | ExampleJSON
    | ExampleSQL
    | ExampleStringFormat

type Msg =
    | BasicExampleJSON
    | BasicExampleSQL
    | BasicExampleStringFormat
    | ExampleJSON of string
    | ExampleSQL of string
    | ExampleStringFormat of string


type ParseResult =
    | JSONParse of ClassifierBuilderResult<JsonValue, JsonValue>
    | SQLParse of ClassifierBuilderResult<TokenType, SQLQuery>
    | StringFormatParse of ClassifierBuilderResult<string, FLexer.Example.Core.StringFormat.Tree>

type Model =
    {   CurrentPage: Page
        CurrentText: string
        ParseResult: ParseResult
    }

    static member ExampleJSON =
        let exampleText = FLexer.Example.Core.JSON.ExampleStrings |> List.choose(fun (shouldBeTrue,json) -> if shouldBeTrue then Some json else None) |> List.sortByDescending(fun t -> t.Length) |> List.head
        {   CurrentPage = Page.ExampleJSON
            CurrentText = exampleText
            ParseResult = ParseResult.JSONParse (FLexer.Example.Core.JSON.ExampleTester exampleText)
        }
        
    static member ExampleSQL =
        let exampleText = FLexer.Example.Core.BasicSQL.ExampleStrings |> List.choose(fun (shouldBeTrue,json) -> if shouldBeTrue then Some json else None) |> List.sortByDescending(fun t -> t.Length) |> List.head
        {   CurrentPage = Page.ExampleSQL
            CurrentText = exampleText
            ParseResult = ParseResult.SQLParse (FLexer.Example.Core.BasicSQL.ExampleTester exampleText)
        }

    static member ExampleStringFormat =
        let exampleText = FLexer.Example.Core.StringFormat.ExampleStrings |> List.choose(fun (shouldBeTrue,json) -> if shouldBeTrue then Some json else None) |> List.sortByDescending(fun t -> t.Length) |> List.head
        {   CurrentPage = Page.ExampleStringFormat
            CurrentText = exampleText
            ParseResult = ParseResult.StringFormatParse (FLexer.Example.Core.StringFormat.ExampleTester exampleText)
        }
