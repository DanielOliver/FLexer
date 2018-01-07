namespace FLexer.Tests

open NUnit.Framework
open FLexer.Core.Tokenizer
open FLexer.Tests.Utility

[<TokenizerDefinedTests>]
type TokenizerDefinedTests () =
    let expectOk = sprintf "Expected '%s' to be accepted"
    let expectError = sprintf "Expected '%s' to be rejected"

    [<Test>]
    member __.``TakeChar - Assure that UpperCaseAlpha is inclusive`` () =
        let checkString = TokenizerStatus.OfString >> Defined.UpperCaseAlpha

        "A" |> checkString |> AssertIsOk (expectOk "A")
        "Z" |> checkString |> AssertIsOk (expectOk "Z")
        "m" |> checkString |> AssertIsError (expectError "m")

    [<Test>]
    member __.``TakeChar - Assure that LowerCaseAlpha is inclusive`` () =
        let checkString = TokenizerStatus.OfString >> Defined.LowerCaseAlpha

        "a" |> checkString |> AssertIsOk (expectOk "a")
        "z" |> checkString |> AssertIsOk (expectOk "z")
        "M" |> checkString |> AssertIsError (expectError "M")

    [<Test>]
    member __.``TakeChar - Assure that Alpha is inclusive`` () =
        let checkString = TokenizerStatus.OfString >> Defined.Alpha

        "a" |> checkString |> AssertIsOk (expectOk "a")
        "z" |> checkString |> AssertIsOk (expectOk "z")
        "A" |> checkString |> AssertIsOk (expectOk "A")
        "Z" |> checkString |> AssertIsOk (expectOk "Z")
        "M" |> checkString |> AssertIsOk (expectOk "M")
        "8" |> checkString |> AssertIsError (expectError "8")