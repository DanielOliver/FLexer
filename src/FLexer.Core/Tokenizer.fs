module FLexer.Core.Tokenizer

/// Any error in consuming characters returns this.
[<RequireQualifiedAccessAttribute>]
type TokenizerError =
    | EOF    
    | MatchFailure
    | RegexMatchFailure
    | ClassifierFailure
    | LookaheadFailure
    | UnconsumedText

/// Keeps track of the Tokenizer as consuming.
type TokenizerStatus =
    {   CurrentChar: int
        Remainder: string
    }

    /// Converts raw string into the beginning TokenizerStatus
    static member OfString text =
        {   TokenizerStatus.CurrentChar = 0
            TokenizerStatus.Remainder = text
        }

/// The raw text, position, and user-defined classification of a token.
type Token<'t> = 
    {   StartCharacter: int
        EndCharacter: int
        Text: string
        Classification: 't
    }

/// A Result of the Tokenizer consumer.
type ConsumerResult = Result<string * TokenizerStatus, TokenizerError>
/// Any consumer of strings should match this signature.
type Consumer = TokenizerStatus -> ConsumerResult

/// Defined ways to combine consumers.
module Operators = 

    /// Given a consumed string and a status, returns an OK with new status and consumed string.
    let Consumed (text: string) (status: TokenizerStatus): ConsumerResult = 
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
        fun tokenizerStatus ->
            let result1 = consumer tokenizerStatus
            match result1 with
            | Ok(_) -> result1
            | Error(_) ->
                consumer2 tokenizerStatus
    /// Consumes zero or more conesecutive instances of the consumer. Never returns error.
    let ZeroOrMore (consumer: Consumer) : Consumer =
        fun tokenizerStatus ->
            let rec takeAnother text status =
                match consumer status with
                | Ok(nextText, nextStatus) ->
                    let addedText = text + nextText
                    takeAnother addedText nextStatus
                | Error(_) -> Consumed text status
            takeAnother System.String.Empty tokenizerStatus            
    /// Consumes one or more conesecutive instances of the consumer. Returns error if none consumed.
    let OneOrMore (consumer: Consumer) : Consumer = 
        fun tokenizerStatus ->
            let rec takeAnother text status isFirst =
                match consumer status with
                | Ok(nextText, nextStatus) ->
                    let addedText = text + nextText
                    takeAnother addedText nextStatus false
                | Error(_) ->
                    match isFirst with
                    | true -> Error TokenizerError.MatchFailure
                    | false -> Consumed text status
            takeAnother System.String.Empty tokenizerStatus true

/// Defined ways to consume characters.
module Consumers = 
    /// Consumes the first match of the given regex that appears at index 0.
    let TakeRawRegex (regex: System.Text.RegularExpressions.Regex) (status: TokenizerStatus): ConsumerResult =
        let matches = regex.Matches(status.Remainder)
        matches 
        |> Seq.cast<System.Text.RegularExpressions.Match>
        |> Seq.tryPick(fun t -> if t.Index = 0 then Some t.Value else None)
        |> (function 
            | Some text -> Operators.Consumed text status 
            | None -> Error TokenizerError.RegexMatchFailure )
            
    /// Consumes the first match of the given regex that appears at index 0.
    let TakeRegex (regex: string) (status: TokenizerStatus): ConsumerResult =
        TakeRawRegex (regex |> System.Text.RegularExpressions.Regex) status

    /// Consumes the given word if the string is a match.
    let TakeWord (word: string) (ignoreCase: bool) (status: TokenizerStatus): ConsumerResult =
        if status.Remainder.Length < word.Length then
            Error TokenizerError.EOF
        else
            let remainderWord = status.Remainder.Substring(0, word.Length)
            let comparisonType = if ignoreCase then System.StringComparison.CurrentCultureIgnoreCase else System.StringComparison.CurrentCulture
            if  remainderWord.Equals(word, comparisonType) then
                Operators.Consumed remainderWord status
            else
                Error TokenizerError.MatchFailure

    /// Consumes one given character.
    let TakeChar character (status: TokenizerStatus): ConsumerResult =
        if status.Remainder.Length = 0 then
            Error TokenizerError.EOF
        else
            if status.Remainder.[0] = character then
                let textResult = status.Remainder.[0].ToString()
                Operators.Consumed textResult status
            else
                Error TokenizerError.MatchFailure

    /// Consumes one character within the given range of characters.
    let TakeCharRange startChar endChar (status: TokenizerStatus): ConsumerResult =
        if status.Remainder.Length = 0 then
            Error TokenizerError.EOF
        else
            let actualChar = status.Remainder.[0]
            if actualChar >= startChar && actualChar <= endChar then
                let textResult = actualChar.ToString()
                Operators.Consumed textResult status
            else
                Error TokenizerError.MatchFailure

module Defined =
    let UpperCaseAlpha = Consumers.TakeCharRange 'A' 'Z'
    let LowerCaseAlpha = Consumers.TakeCharRange 'a' 'z'
    let Alpha = Operators.Or UpperCaseAlpha LowerCaseAlpha
