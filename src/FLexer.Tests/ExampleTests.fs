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
        FLexer.Example.BasicSQL.ExampleStrings
        |> List.iter(fun (shouldAccept, stringToTest) ->
            stringToTest
            |> FLexer.Example.BasicSQL.ExampleTester
            |> (testResult shouldAccept stringToTest)
        )    
        

        