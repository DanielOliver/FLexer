namespace FLexer.Core

open Tokenizer

/// The result of a ClassifierBuilder computation expression
type ClassifierBuilderResult<'t,'b> = Result<'b * ClassifierStatus<'t>, ClassifierError<'t>>

/// Given a ClassifierStatus, the computation expression returns a ClassifierBuilderResult
type ClassifierBuilderFunction<'t,'b> = ClassifierStatus<'t> -> ClassifierBuilderResult<'t, 'b>

/// A flag to pass to ClassifierBuilder compution expression that signifies no result is returned by "do!"
type ClassifierBuilderEmptyFlag<'t> =
    | Discard of Tokenizer.Consumer
    | Accept of Classifier<'t>
    | Failure
    | FailureWith of TokenizerError

/// A flag to pass to ClassifierBuilder compution expression that signifies a list is returned from "let!"
type ClassifierBuilderResultMultipleFlag<'t,'c> =
    | ZeroOrOne of ClassifierBuilderFunction<'t,'c>
    | ZeroOrMore of ClassifierBuilderFunction<'t,'c>
    | OneOrMore of ClassifierBuilderFunction<'t,'c>
    | OneOrMoreWhile of ClassifierBuilderFunction<'t,'c> * (Token<'t> -> bool)

/// A flag to pass to ClassifierBuilder compution expression that signifies only one item is returned from multiple ClassifierBuilder
type BuilderChoice<'t,'c> =
    | BuilderChoice of ClassifierBuilderFunction<'t,'c> list

/// A flag to pass to ClassifierBuilder compution expression that signifies only one item is returned from multiple Classifiers
type Choice<'t> =
    | Choice of Classifier<'t> list
        

/// A Computation Expression to handle Result.bind of Classifiers and to hide passing "ClassifierStatus" around.
type ClassifierBuilder<'t, 'b>(initialStatus: ClassifierStatus<'t>) =
    let mutable currentStatus = initialStatus
    
    /// Example: let! tokenType = Classifier.name "SELECT" SELECT
    member this.Bind(classifier: Classifier<'t>, f: 't -> ClassifierBuilderResult<'t, 'b>): ClassifierBuilderResult<'t, 'b> =
        currentStatus
        |> classifier
        |> Result.bind(fun t -> 
            currentStatus <- t
            t.Consumed.Head.Classification |> f)

    /// Example: let! valueType = Classifier.name "SELECT" SELECTClassifierBuilder       
    member this.Bind<'c>(classifier: ClassifierBuilderFunction<'t, 'c>, f: 'c -> ClassifierBuilderResult<'t, 'b>): ClassifierBuilderResult<'t, 'b> =
        currentStatus
        |> classifier
        |> Result.bind(fun (value, t) -> 
            currentStatus <- t
            value |> f)
            
    /// Example: do! Discard WHITESPACE 
    member this.Bind(flag: ClassifierBuilderEmptyFlag<'t>, f: unit -> ClassifierBuilderResult<'t, 'b>): ClassifierBuilderResult<'t, 'b> =
        match flag with 
        | Discard consumer ->
            currentStatus
            |> Classifier.discard consumer
            |> Result.bind(fun t ->
                currentStatus <- t
                f())

        | Accept classifier ->
            currentStatus
            |> classifier
            |> Result.bind(fun t ->
                currentStatus <- t
                f())
        | Failure ->
            ClassifierBuilderResult.Error(ClassifierError<_>.OfTokenizerError currentStatus (Some Tokenizer.TokenizerError.ClassifierFailure))
        | FailureWith tokenizerError ->
            ClassifierBuilderResult.Error(ClassifierError<_>.OfTokenizerError currentStatus (Some tokenizerError))
            
    /// Example: let! firstValueType = [Builder1, Builder2]
    member this.Bind<'c>(builders: ClassifierBuilderFunction<'t,'c> list, f: _ -> ClassifierBuilderResult<'t, 'b>): ClassifierBuilderResult<'t, 'b> =
        let firstError = builders.Head currentStatus
        match firstError with 
        | Ok _ -> firstError
        | _ ->
            builders
            |> List.tryPick(fun t ->
                match t currentStatus with
                | Error _ -> None
                | x -> Some x)
            |> Option.defaultValue firstError
        |> Result.bind(fun (value, status) ->
                currentStatus <- status
                value |> f)
                
    /// Example: let! firstValueType = [Builder1, Builder2]
    member this.Bind<'c>(buildersChoice: BuilderChoice<'t, 'c>, f: 'c -> ClassifierBuilderResult<'t, 'b>): ClassifierBuilderResult<'t, 'b> =
        let (BuilderChoice builders) = buildersChoice
        let firstError = builders.Head currentStatus
        match firstError with 
        | Ok _ -> firstError
        | _ ->
            builders
            |> List.tryPick(fun t ->
                match t currentStatus with
                | Error _ -> None
                | x -> Some x)
            |> Option.defaultValue firstError
        |> Result.bind(fun (value, status) ->
                currentStatus <- status
                value |> f)

    /// Example: let! tokenType = Choice [ Classifier.name TokenType.From FROM; Classifier.map TokenType.ColumnName IDENTIFIER ]
    member this.Bind<'c>(choice: Choice<'t>, f: 't -> ClassifierBuilderResult<'t, 'b>): ClassifierBuilderResult<'t, 'b> =
        let (Choice choices) = choice
        let firstError = choices.Head currentStatus
        match firstError with 
        | Ok _ -> firstError
        | _ ->
            choices
            |> List.tryPick(fun t ->
                match t currentStatus with
                | Error _ -> None
                | x -> Some x)
            |> Option.defaultValue firstError
        |> Result.bind(fun status ->
                currentStatus <- status
                status.Consumed.Head.Classification |> f)

    /// Example: let! columns = OneOrMore AcceptColumnName
    member this.Bind<'c>(flag: ClassifierBuilderResultMultipleFlag<'t, 'c>, f: 'c list -> ClassifierBuilderResult<'t, 'b>): ClassifierBuilderResult<'t, 'b> =
        let acceptZero = match flag with | ZeroOrMore _ -> true | _ -> false
        let predicate = match flag with | OneOrMoreWhile(_, predicate) -> predicate | _ -> (fun _ -> true)
        let maxOne = match flag with | ZeroOrOne _ -> true | _ -> false
        
        match flag with 
        | ZeroOrOne classifier
        | ZeroOrMore classifier
        | OneOrMore classifier
        | OneOrMoreWhile (classifier, _) ->
            let mutable continueLoop = true
            let mutable items = []
            let mutable statusList = [ currentStatus ]
            while continueLoop do
                if maxOne && items.Length = 1 then
                    continueLoop <- false
                else
                    match classifier statusList.Head with
                    | Ok(value, result) ->
                        if predicate result.Consumed.Head then
                            items <- value :: items
                            statusList <- result :: statusList
                        else
                            continueLoop <- false
                    | Error(err) ->
                        continueLoop <- false


            let finalError = ClassifierBuilderResult.Error(ClassifierError<_>.OfTokenizerError currentStatus (Some Tokenizer.TokenizerError.ClassifierFailure))
            if acceptZero && items.IsEmpty then
                List.empty |> f
            else                
                let rec tryItems (itemList: 'c list) (statusList: ClassifierStatus<'t> list) =
                    if itemList.IsEmpty then
                        if acceptZero then
                            List.Empty |> f
                        else
                            finalError
                    else
                        currentStatus <- statusList.Head
                        items
                        |> f
                        |> (function
                            | Error err ->
                                tryItems itemList.Tail statusList.Tail
                            | Ok _ as x -> x)
                tryItems items statusList
            
    /// Example: return "IamString"
    member this.Return(x: 'b): ClassifierBuilderResult<'t, 'b> = 
        Ok( x, currentStatus)
    
    /// Example: return Failure
    member this.Return(x: ClassifierBuilderEmptyFlag<'t>): ClassifierBuilderResult<'t, 'b> =
        match x with
        | FailureWith tokenizerError ->
            ClassifierBuilderResult.Error(ClassifierError<_>.OfTokenizerError currentStatus (Some tokenizerError))
        | Failure
        | _ ->
            ClassifierBuilderResult.Error(ClassifierError<_>.OfTokenizerError currentStatus (Some Tokenizer.TokenizerError.ClassifierFailure))
        