module FLexer.Core.Descent

/// Any error in consuming characters returns this.
[<RequireQualifiedAccessAttribute>]
type DescentError =
    | EOF    
    | MatchFailure

/// Keeps track of the descent as consuming.
type DescentStatus =
    {   CurrentChar: int
        Remainder: string
    }

/// A Result of the Descent consumer.
type ConsumerResult = Result<string * DescentStatus, DescentError>
/// Any consumer of strings should match this signature.
type Consumer = DescentStatus -> ConsumerResult

/// Defined ways to combine consumers.
module Operators = 

    /// Given a consumed string and a status, returns an OK with new status and consumed string.
    let Consumed (text: string) (status: DescentStatus): ConsumerResult = 
        let remainder = status.Remainder.Substring(text.Length)
        Ok(text, 
            {   CurrentChar = status.CurrentChar + text.Length
                Remainder = remainder
            })

    ///Chains two Consumers together.
    let Next (consumer: Consumer) (previous: ConsumerResult) : ConsumerResult =
        previous 
        |> Result.bind(fun (text, status) -> 
            status
            |> consumer
            |> Result.map(fun (text2, status2) ->
                text + text2, status2
            )
        )
    /// Tries the first consumer, thenthe second if the first errors. Returns the second's error if both error.
    let Or (consumer: Consumer) (consumer2: Consumer) : Consumer =
        fun descentStatus ->
            let result1 = consumer descentStatus
            match result1 with
            | Ok(_) -> result1
            | Error(_) ->
                consumer2 descentStatus

/// Defined ways to consume characters.
module Consumers = 

    /// Consumes one given character.
    let TakeChar character (status: DescentStatus): ConsumerResult =
        if status.Remainder.Length = 0 then
            Error DescentError.EOF
        else
            if status.Remainder.[0] = character then
                let textResult = status.Remainder.[0].ToString()
                Operators.Consumed textResult status
            else
                Error DescentError.MatchFailure

    /// Consumes one character within the given range of characters.
    let TakeCharRange startChar endChar (status: DescentStatus): ConsumerResult =
        if status.Remainder.Length = 0 then
            Error DescentError.EOF
        else
            let actualChar = status.Remainder.[0]
            if actualChar >= startChar && actualChar <= endChar then
                let textResult = actualChar.ToString()
                Operators.Consumed textResult status
            else
                Error DescentError.MatchFailure


let charAZ = Consumers.TakeCharRange 'A' 'Z'
let charaz = Consumers.TakeCharRange 'a' 'z'
let alpha = charaz >> Operators.Next charAZ
let alpha2 = Operators.Or charAZ charaz
