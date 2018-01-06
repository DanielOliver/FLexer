namespace FLexer.Core

/// Keeps track as Classifying tokens.
type ClassifierStatus<'t> =
    {   Consumed: Tokenizer.Token<'t> list
        CurrentChar: int
        Remainder: string
    }

    static member OfString initialValue =
        {   Consumed = List.empty<_>
            CurrentChar = 0
            Remainder = initialValue
        }

    static member FromTokenizerStatus consumed (tokenizerStatus: Tokenizer.TokenizerStatus) =
        {   Consumed = consumed
            CurrentChar = tokenizerStatus.CurrentChar
            Remainder = tokenizerStatus.Remainder
        }
        
    member this.TokenizerStatus =
        {   Tokenizer.TokenizerStatus.CurrentChar = this.CurrentChar
            Tokenizer.TokenizerStatus.Remainder = this.Remainder
        }

/// A Result of the Classifier consumer.
type ClassifierResult<'t> = Result<ClassifierStatus<'t>, Tokenizer.TokenizerError>
/// Classifies raw text as a token type.
type Classifier<'t> = ClassifierStatus<'t> -> ClassifierResult<'t>

module Classifier =

    let private returnClassifierResult (tokenizerStatus: Tokenizer.TokenizerStatus) newToken (oldClassifierStatus: ClassifierStatus<_>) =
        {   ClassifierStatus.Consumed = newToken :: oldClassifierStatus.Consumed
            ClassifierStatus.CurrentChar = tokenizerStatus.CurrentChar
            ClassifierStatus.Remainder = tokenizerStatus.Remainder
        }


    /// Names this token a const value, no matter what. 
    let name name (consumerResult: Tokenizer.Consumer): Classifier<_> =
        fun (classifierStatus: ClassifierStatus<_>) ->
            classifierStatus.TokenizerStatus
            |> consumerResult
            |> Result.map(fun (text, status) ->
                let newToken =
                    {   Tokenizer.Token.Classification = name
                        Tokenizer.Token.StartCharacter = status.CurrentChar - text.Length
                        Tokenizer.Token.EndCharacter = status.CurrentChar - 1
                        Tokenizer.Token.Text = text
                    }
                returnClassifierResult status newToken classifierStatus
            )

    /// Maps this token's text to a label.
    let map mapper (consumerResult: Tokenizer.Consumer): Classifier<_> =
        fun (classifierStatus: ClassifierStatus<_>) ->
            classifierStatus.TokenizerStatus
            |> consumerResult
            |> Result.map(fun (text, status) ->
                let newToken =
                    {   Tokenizer.Token.Classification = text |> mapper
                        Tokenizer.Token.StartCharacter = status.CurrentChar - text.Length
                        Tokenizer.Token.EndCharacter = status.CurrentChar - 1
                        Tokenizer.Token.Text = text
                    }
                returnClassifierResult status newToken classifierStatus
            )
                

    let mapValid mapper (consumerResult: Tokenizer.Consumer): Classifier<_> =
        fun (classifierStatus: ClassifierStatus<_>) ->
            classifierStatus.TokenizerStatus
            |> consumerResult
            |> Result.bind(fun (text, status) ->
                text 
                |> mapper
                |> Result.map(fun classificiation ->
                    let newToken = 
                        {   Tokenizer.Token.Classification = classificiation
                            Tokenizer.Token.StartCharacter = status.CurrentChar - text.Length
                            Tokenizer.Token.EndCharacter = status.CurrentChar - 1
                            Tokenizer.Token.Text = text 
                        }
                    returnClassifierResult status newToken classifierStatus
                )
            )
