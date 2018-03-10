namespace FLexer.Core

/// Keeps track as Classifying tokens.
type ClassifierStatus<'t> =
    {   Consumed: Tokenizer.Token<'t> list
        ConsumedWords: string list
        CurrentChar: int
        Remainder: string
    }

    static member OfString initialValue =
        {   Consumed = List.empty<_>
            ConsumedWords = List.empty
            CurrentChar = 0
            Remainder = initialValue
        }
        
    member this.TokenizerStatus =
        {   Tokenizer.TokenizerStatus.CurrentChar = this.CurrentChar
            Tokenizer.TokenizerStatus.Remainder = this.Remainder
        }

    member this.Classification = this.Consumed.Head.Classification
    member this.ConsumedText = this.ConsumedWords.Head
    member this.ConsumedLength = this.CurrentChar


type ClassifierError<'t> =
    {   LastStatus: ClassifierStatus<'t>
        TokenizerError: Tokenizer.TokenizerError option
    }
    
    member this.ConsumedLength = this.LastStatus.CurrentChar

    static member OfTokenizerError classifierStatus tokenizerError =
        {   LastStatus = classifierStatus
            TokenizerError = tokenizerError
        }

/// A Result of the Classifier consumer.
type ClassifierResult<'t> = Result<ClassifierStatus<'t>, ClassifierError<'t>>
/// Classifies raw text as a token type.
type Classifier<'t> = ClassifierStatus<'t> -> ClassifierResult<'t>


module Classifier =

    let private returnClassifierResult (tokenizerStatus: Tokenizer.TokenizerStatus) newToken (oldClassifierStatus: ClassifierStatus<_>) =
        {   ClassifierStatus.Consumed = newToken :: oldClassifierStatus.Consumed
            ClassifierStatus.ConsumedWords = newToken.Text :: oldClassifierStatus.ConsumedWords
            ClassifierStatus.CurrentChar = tokenizerStatus.CurrentChar
            ClassifierStatus.Remainder = tokenizerStatus.Remainder
        }

        
    let private discardClassifierResult (tokenizerStatus: Tokenizer.TokenizerStatus) text (oldClassifierStatus: ClassifierStatus<_>) =
        if System.String.IsNullOrEmpty text then
            oldClassifierStatus
        else
        {   ClassifierStatus.Consumed = oldClassifierStatus.Consumed
            ClassifierStatus.ConsumedWords = text :: oldClassifierStatus.ConsumedWords
            ClassifierStatus.CurrentChar = tokenizerStatus.CurrentChar
            ClassifierStatus.Remainder = tokenizerStatus.Remainder
        }
        
    let private discardClassifierStatus (consumerResult: Tokenizer.Consumer) (classifierStatus: ClassifierStatus<_>): ClassifierResult<_> =
        classifierStatus.TokenizerStatus
        |> consumerResult
        |> Result.map(fun (text, status) -> discardClassifierResult status text classifierStatus
        )
        |> Result.mapError (Some >> ClassifierError<_>.OfTokenizerError classifierStatus)

    let private mapClassifierStatus (consumerResult: Tokenizer.Consumer) (mapTextToClassification: string -> _) (classifierStatus: ClassifierStatus<_>): ClassifierResult<_> =
        classifierStatus.TokenizerStatus
        |> consumerResult
        |> Result.map(fun (text, status) ->
            let newToken =
                {   Tokenizer.Token.Classification = mapTextToClassification text
                    Tokenizer.Token.StartCharacter = status.CurrentChar - text.Length
                    Tokenizer.Token.EndCharacter = status.CurrentChar - 1
                    Tokenizer.Token.Text = text
                }
            returnClassifierResult status newToken classifierStatus
        )
        |> Result.mapError (Some >> ClassifierError<_>.OfTokenizerError classifierStatus)
        
    let private bindClassifierStatus (consumerResult: Tokenizer.Consumer) (bindTextToClassification: string -> Result<_, Tokenizer.TokenizerError>) (classifierStatus: ClassifierStatus<_>): ClassifierResult<_> =
        classifierStatus.TokenizerStatus
        |> consumerResult
        |> Result.bind(fun (text, status) ->
            text 
            |> bindTextToClassification
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
        |> Result.mapError (Some >> ClassifierError<_>.OfTokenizerError classifierStatus)

    /// Names this token a const value, no matter what. 
    let name name (consumerResult: Tokenizer.Consumer): Classifier<_> =
        mapClassifierStatus consumerResult (fun _ -> name)

    /// Maps this token's text to a label.
    let map mapper (consumerResult: Tokenizer.Consumer): Classifier<_> =
        mapClassifierStatus consumerResult mapper
                
    /// Map's this token's text to a label, or accepts failure to tokenize.
    let mapValid mapper (consumerResult: Tokenizer.Consumer): Classifier<_> =
        bindClassifierStatus consumerResult mapper

    /// Discard's this token and moves on. Good for whitespace and unwanted characters.
    let discard (consumerResult: Tokenizer.Consumer): Classifier<_> =
        discardClassifierStatus consumerResult
    
    let tryMultiple (classifiers: Classifier<_> list): Classifier<_> =
        fun (classifierStatus: ClassifierStatus<_>) ->
            let firstError = classifiers.Head classifierStatus
            match firstError with 
            | Ok _ -> firstError
            | _ ->
                classifiers 
                |> List.tryPick(fun t ->
                    match t classifierStatus with
                    | Error err -> None
                    | x -> Some x)
                |> Option.defaultValue firstError

