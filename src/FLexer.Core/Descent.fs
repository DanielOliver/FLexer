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

/// Any consumer of strings should match this.
type Consumer = DescentStatus -> Result<string * DescentStatus, DescentError>

/// Given a consumed string and a status, returns an OK with new status and consumed string.
let private Consumed (text: string) (status: DescentStatus) = 
    let remainder = status.Remainder.Substring(text.Length)
    Ok(text, 
        {   CurrentChar = status.CurrentChar + text.Length
            Remainder = remainder
        })

/// Consumes one given character.
let TakeChar character (status: DescentStatus) =
    if status.Remainder.Length = 0 then
        Error DescentError.EOF
    else
        if status.Remainder.[0] = character then
            let textResult = status.Remainder.[0].ToString()
            Consumed textResult status
        else
            Error DescentError.MatchFailure

/// Consumes one character within the given range of characters.
let TakeCharRange startChar endChar (status: DescentStatus) =
    if status.Remainder.Length = 0 then
        Error DescentError.EOF
    else
        let actualChar = status.Remainder.[0]
        if actualChar >= startChar && actualChar <= endChar then
            let textResult = actualChar.ToString()
            Consumed textResult status
        else
            Error DescentError.MatchFailure
