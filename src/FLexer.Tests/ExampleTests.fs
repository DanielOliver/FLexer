namespace FLexer.Tests

open NUnit.Framework
open FLexer.Core
open FLexer.Tests.Utility

[<ExampleTests>]
type ExampleTests () =

    let testResult shouldAccept testString result =
        if shouldAccept then
            result |> AssertIsOk (sprintf "Should Accept: %s" testString)
        else
            result |> AssertIsError (sprintf "Should Reject: %s" testString)
            
    [<Test>]
    member __.``Verify BasicSQL Examples`` () = 
        FLexer.Example.Core.BasicSQL.ExampleStrings
        |> List.iter(fun (shouldAccept, stringToTest) ->
            stringToTest
            |> FLexer.Example.Core.BasicSQL.ExampleTester
            |> (testResult shouldAccept stringToTest)
        )
        
    [<Test>]
    member __.``Verify JSON Examples`` () = 
        FLexer.Example.Core.JSON.ExampleStrings
        |> List.iter(fun (shouldAccept, stringToTest) ->
            stringToTest
            |> FLexer.Example.Core.JSON.ExampleTester
            |> (testResult shouldAccept stringToTest)
        )
        
    [<Test>]
    member __.``Verify StringFormat Examples`` () = 
        FLexer.Example.Core.StringFormat.ExampleStrings
        |> List.iter(fun (shouldAccept, stringToTest) ->
            stringToTest
            |> FLexer.Example.Core.StringFormat.ExampleTester
            |> (testResult shouldAccept stringToTest)
        )
        
