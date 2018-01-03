namespace FLexer.Tests

open NUnit.Framework
open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Tests.Utility


[<ClassifierTests>]
type ClassifierTests () =
    let givenOkStatus = Result.Ok("text", { TokenizerStatus.CurrentChar = 4; Remainder = "remaining" } )
    let givenFailedStatus = Result.Error TokenizerError.EOF

    [<Test>]
    member __.``Verify name classification`` () = 
        let classified = Classifier.name "namedToken" givenOkStatus
        classified
        |> ExpectOk (fun (token, status) -> Assert.AreEqual("namedToken", token.Classification))
        
    [<Test>]
    member __.``Verify name classification failure`` () = 
        let classified = Classifier.name "namedToken" givenFailedStatus
        classified
        |> AssertIsError "Expected error, not named."

    [<Test>]
    member __.``Verify mapper classification`` () = 
        let classified = Classifier.map (fun t -> if t = "text" then "one" else "two") givenOkStatus
        classified
        |> ExpectOk (fun (token, status) -> Assert.AreEqual("one", token.Classification))

        let classified2 = Classifier.map (fun t -> if t = "text234234234" then "one" else "two") givenOkStatus
        classified2
        |> ExpectOk (fun (token, status) -> Assert.AreEqual("two", token.Classification))
        
    [<Test>]
    member __.``Verify mapper classification failure`` () = 
        let classified = Classifier.map (fun _ -> "text") givenFailedStatus
        classified
        |> AssertIsError "Expected error, not named."
