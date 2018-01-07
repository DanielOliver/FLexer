namespace FLexer.Tests

open NUnit.Framework
open FLexer.Core.Tokenizer

[<TokenizerOperatorTests>]
type TokenizerOperatorTests () =
    [<Test>]
    member __.``TakeChar - Assure that ZeroOrMany consumes`` () =
        let checkString = TokenizerStatus.OfString >> (Operators.ZeroOrMore Defined.Alpha)

        "abc wasd" |> checkString |> Utility.ExpectOkText "abc"
        "zyx wasd" |> checkString |> Utility.ExpectOkText "zyx"
        "  zyx wasd" |> checkString |> Utility.ExpectOkText ""
