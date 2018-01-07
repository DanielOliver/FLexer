namespace FLexer.Tests

open NUnit.Framework
open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Tests.Utility


[<ClassifierTests>]
type ClassifierTests () =
    let givenOkStatus : ClassifierStatus<string> = { ClassifierStatus.CurrentChar = 4; Remainder = "remaining"; ConsumedWords = List.empty; Consumed = List.empty }

    [<Test>]
    member __.``Verify name classification`` () = 
        givenOkStatus
        |> (Classifier.name "namedToken" (Consumers.TakeWord "remaining" true))
        |> ExpectOk (fun status -> Assert.AreEqual("namedToken", status.Consumed.Head.Classification))
        
    [<Test>]
    member __.``Verify name classification failure`` () = 
        givenOkStatus
        |> (Classifier.name "namedToken" (Consumers.TakeWord "remainingasdg" true))
        |> AssertIsError "Expected error, not named."

    [<Test>]
    member __.``Verify mapper classification`` () =
        givenOkStatus
        |> (Classifier.map (fun t -> if t = "text" then "one" else "two") (Consumers.TakeWord "remaining" true))
        |> ExpectOk (fun status -> Assert.AreEqual("two", status.Consumed.Head.Classification))
        
        givenOkStatus
        |> (Classifier.map (fun t -> if t = "remaining" then "one" else "two") (Consumers.TakeWord "remaining" true))
        |> ExpectOk (fun status -> Assert.AreEqual("one", status.Consumed.Head.Classification))

