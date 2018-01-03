namespace FLexer.Core

/// A Result of the Classifier consumer.
type ClassifierResult<'t> = Result<Tokenizer.Token<'t> * Tokenizer.TokenizerStatus, Tokenizer.TokenizerError>
/// Classifies raw text as a token type.
type Classifier<'t> = Tokenizer.ConsumerResult -> ClassifierResult<'t>

module Classifier =
    /// Names this token a const value, no matter what. 
    let name name (consumerResult: Tokenizer.ConsumerResult): ClassifierResult<_> =
        consumerResult
        |> Result.map(fun (text, status) ->
            {   Tokenizer.Token.Classification = name
                Tokenizer.Token.StartCharacter = status.CurrentChar - text.Length
                Tokenizer.Token.EndCharacter = status.CurrentChar - 1
                Tokenizer.Token.Text = text
            }, status
        )

    /// Maps this token's text to a label.
    let map mapper (consumerResult: Tokenizer.ConsumerResult): ClassifierResult<_> =
        consumerResult
        |> Result.map(fun (text, status) ->
            {   Tokenizer.Token.Classification = text |> mapper
                Tokenizer.Token.StartCharacter = status.CurrentChar - text.Length
                Tokenizer.Token.EndCharacter = status.CurrentChar - 1
                Tokenizer.Token.Text = text 
            }, status
        )

    let mapValid mapper (consumerResult: Tokenizer.ConsumerResult) =
        consumerResult
        |> Result.bind(fun (text, status) ->
            text 
            |> mapper
            |> Result.map(fun classificiation ->
                {   Tokenizer.Token.Classification = classificiation
                    Tokenizer.Token.StartCharacter = status.CurrentChar - text.Length
                    Tokenizer.Token.EndCharacter = status.CurrentChar - 1
                    Tokenizer.Token.Text = text 
                }, status
            )
        )            