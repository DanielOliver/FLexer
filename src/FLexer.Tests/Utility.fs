module FLexer.Tests.Utility

open NUnit.Framework

let IsOk (result: Result<_,_>) =
    match result with
    | Ok(_) -> true
    | Error(_) -> false

let IsError (result: Result<_,_>) =
    result |> IsOk |> not

let AssertIsOk message result =
    Assert.IsTrue(IsOk result, message)


let ExpectOkText expected result =
    match result with
    | Ok(text, _) -> Assert.AreEqual(expected, text)
    | Error(_) -> Assert.Fail("Expected OK")

let AssertIsError message result =
    Assert.IsTrue(IsError result, message)