namespace FLexer.Tests

open NUnit.Framework
open FLexer.Core.Tokenizer

[<TokenizerConsumerTests>]
type TokenizerConsumerTests () =

    let basicStatus =  "SELECT * FROM TESTS;" |> TokenizerStatus.OfString

    [<Test>]
    member __.``TakeChar - Assure that one char is consumed`` () =
        match Consumers.TakeChar 'S' basicStatus with 
        | Ok(text, status) ->
            Assert.AreEqual("S", text)
            Assert.AreEqual(1, status.CurrentChar)
            Assert.AreEqual("ELECT * FROM TESTS;", status.Remainder)
        | Error _ ->
            Assert.Fail("Should consume one 'S' character.")

    [<Test>]
    member __.``TakeChar - Assure that one char is NOT consumed`` () =
        Consumers.TakeChar 'M' basicStatus |> Utility.AssertIsError "Should NOT consume character."
            
    [<Test>]
    member __.``TakeCharRange - Assure that one char is consumed`` () =
        match Consumers.TakeCharRange 'R' 'Y' basicStatus with 
        | Ok(text, status) ->
            Assert.AreEqual("S", text)
            Assert.AreEqual(1, status.CurrentChar)
            Assert.AreEqual("ELECT * FROM TESTS;", status.Remainder)
        | Error _ ->
            Assert.Fail("Should consume one 'S' character.")

    [<Test>]
    member __.``TakeCharRange - Assure that one char is NOT consumed`` () =
        Consumers.TakeCharRange 'M' 'P' basicStatus |> Utility.AssertIsError "Should NOT consume character."  
            
    [<Test>]
    member __.``TakeRegex - Assure that only first index is matched`` () =
        match Consumers.TakeRegex "SEL" basicStatus with 
        | Ok(text, status) ->
            Assert.AreEqual("SEL", text)
            Assert.AreEqual(3, status.CurrentChar)
            Assert.AreEqual("ECT * FROM TESTS;", status.Remainder)
        | Error _ ->
            Assert.Fail("Should consume 'SEL'.")

        Consumers.TakeRegex "E" basicStatus |> Utility.AssertIsError "E is not at index 0."

        
    [<Test>]
    member __.``TakeRegex - Assure that complex regex is matched`` () =
        match Consumers.TakeRegex "S(E(L)*)*CT" basicStatus with 
        | Ok(text, status) ->
            Assert.AreEqual("SELECT", text)
            Assert.AreEqual(6, status.CurrentChar)
            Assert.AreEqual(" * FROM TESTS;", status.Remainder)
        | Error _ ->
            Assert.Fail("Should consume 'SELECT'.")