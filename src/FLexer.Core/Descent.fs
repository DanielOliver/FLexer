module FLexer.Core.Descent

[<RequireQualifiedAccessAttribute>]
type DescentError =
    | EOF    
    | MatchFailure

type DescentStatus =
    {   CurrentChar: int
        Remainder: string
    }

type Consumer = DescentStatus -> Result<string * DescentStatus, DescentError>


let private Consumed (text: string) (status: DescentStatus) = 
    let remainder = status.Remainder.Substring(text.Length)
    Ok(text, 
        {   CurrentChar = status.CurrentChar + text.Length
            Remainder = remainder
        })

let TakeChar character (status: DescentStatus) =
    if status.Remainder.Length = 0 then
        Error DescentError.EOF
    else
        if status.Remainder.[0] = character then
            let textResult = status.Remainder.[0].ToString()
            Consumed textResult status
        else
            Error DescentError.MatchFailure

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


