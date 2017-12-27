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

let AssertIsError message result =
    Assert.IsTrue(IsError result, message)