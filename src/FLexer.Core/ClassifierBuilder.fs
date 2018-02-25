namespace FLexer.Core

open FLexer.Core
open Tokenizer

/// The result of a ClassifierBuilder computation expression
type ClassifierBuilderResult<'a,'b> = Result<'b * ClassifierStatus<'a>, ClassifierError<'a>>

/// Given a ClassifierStatus, the computation expression returns a ClassifierBuilderResult
type ClassifierBuilderFunction<'a,'b,'c> = 'b * ClassifierStatus<'a> -> ClassifierBuilderResult<'a, 'c>

/// Given a ClassifierStatus and a Classifier, the computation expression returns a ClassifierBuilderResult
type ClassifierBuilderContinuationFromStatus<'a,'b,'c,'d> = ClassifierStatus<'a> -> ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'b>

/// Given a Classifier, the computation expression returns a ClassifierBuilderResult
type ClassifierBuilderContinuation<'a,'b,'c,'d> = ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'b>

/// Operate on Classifier Results
module ClassifierBuilderResult =
    let mapValue mapper (value: 'b, status: ClassifierStatus<'a>) =
        (mapper value, status)

    let map mapper (result: ClassifierBuilderResult<_,_>): ClassifierBuilderResult<_,_> =
        result
        |> Result.map(fun (b, status) -> mapper b, status)

    let continueError (error: ClassifierBuilderResult<_,_>): ClassifierBuilderResult<_,_> =
        match error with
        | Ok _ as x -> x
        | Error err -> Error err

    let bind (binder: ClassifierBuilderFunction<'a, 'b, 'c>) (result: ClassifierBuilderResult<'a,'b>): ClassifierBuilderResult<'a,'c> =
        result 
        |> Result.bind binder

    let defaultIfError (defaultValue: unit -> ClassifierBuilderResult<'a,'b>) (result: ClassifierBuilderResult<'a,'b>): ClassifierBuilderResult<'a,'b> =
        match result with
        | Ok _ -> result
        | Error _ -> defaultValue() 
        

/// An alias for continuation functions.
module Continuation =
    let None: ClassifierBuilderFunction<_,_,_> = ClassifierBuilderResult.Ok

    let Final: ClassifierBuilderFunction<_,_,_> =
        fun (value, status) ->
            if status.Remainder |> System.String.IsNullOrEmpty |> not then
                ClassifierBuilderResult.Error (ClassifierError<_>.OfTokenizerError status (Some Tokenizer.TokenizerError.UnconsumedText))
            else
                ClassifierBuilderResult.Ok(value, status)
            

/// A computation expression to compose functions with
type SubClassifierBuilder<'a,'b,'c>(continuation: ClassifierBuilderFunction<'a, 'b, 'c> ) =

    member this.Bind(classifierResult: ClassifierResult<'a>, f: ClassifierStatus<'a> -> ClassifierBuilderResult<'a,'c>): ClassifierBuilderResult<'a, 'c> =
        classifierResult
        |> Result.bind f


    member this.Bind<'d, 'e>(classifier: ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'e>, f: ClassifierBuilderFunction<'a, 'd, 'c>): ClassifierBuilderResult<'a,'e> =
        f
        |> classifier

        
    member this.Return(value: 'b, status: ClassifierStatus<'a>): ClassifierBuilderResult<'a,'c> =
        (value, status)
        |> ClassifierBuilderResult.Ok
        |> Result.bind continuation

        
    member this.ReturnFrom(status: ClassifierStatus<'a>): ClassifierBuilderResult<'a,'c> =
        ClassifierError<'a>.OfTokenizerError status (Some TokenizerError.LookaheadFailure)
        |> ClassifierBuilderResult.Error

        
    member this.ReturnFrom(classifier: ClassifierBuilderFunction<'a, 'b, 'c> -> ClassifierBuilderResult<'a, 'c>): ClassifierBuilderResult<'a,'c> =
        classifier continuation

    
module Classifiers = 
    let root() = SubClassifierBuilder(Continuation.Final)
    let sub continuation = SubClassifierBuilder(continuation)


/// Common patterns for functions that conform to ClassifierBuilderFunction
module ClassifierFunction =
    /// Impromptu conversion
    let private classifierToBuilderFunction (classifier: ClassifierStatus<'a> -> ClassifierResult<'a>) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, string, 'c>) =
        classifier status |> Result.bind (fun (t: ClassifierStatus<'a>) -> continuation (t.ConsumedText, t))

    /// Replacement for single case discriminated union.
    let PickOne<'a,'b,'c,'d> (classifiers: ClassifierBuilderContinuationFromStatus<'a,'b,'c,'d> list) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, 'd, 'c>) =
        let rec tryClassifier remainingClassifiers =
            match remainingClassifiers with
            | [] -> ClassifierBuilderResult.Error (ClassifierError<'a>.OfTokenizerError status (Some Tokenizer.TokenizerError.LookaheadFailure))
            | classifier :: tail ->
                match classifier status continuation with
                | Ok _ as x -> x
                | Error _ ->
                    tryClassifier tail
        tryClassifier classifiers
        
    /// Replacement for single case discriminated union.
    let PickOneConsumer<'a,'c> (classifiers: (ClassifierStatus<'a> -> ClassifierResult<'a>) list) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, string, 'c>) =
        PickOne (classifiers |> List.map classifierToBuilderFunction) status continuation
        
    /// Replacement for single case discriminated union.
    let ZeroOrOne<'a,'c,'d> (classifier: ClassifierBuilderContinuationFromStatus<'a,'c,'c,'d>) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, 'd option, 'c>) =
        match classifier status (ClassifierBuilderResult.mapValue (Some) >> continuation) with
        | Ok _ as x -> x
        | Error _ -> (None, status) |> continuation
    
    /// Replacement for single case discriminated union.
    let ZeroOrOneConsumer<'a,'c> (classifier: ClassifierStatus<'a> -> ClassifierResult<'a>) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, string option, 'c>) =
        ZeroOrOne (classifierToBuilderFunction classifier) status continuation

    /// Replacement for single case discriminated union.
    let ZeroOrMore<'a,'c,'d> (classifier: ClassifierBuilderContinuationFromStatus<'a,'c,'c,'d>) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, 'd list, 'c>) =
        let rec RecursiveRule valueList status continuation =
            Classifiers.sub continuation {
                let! (tryValue, status) = ZeroOrOne classifier status
                match tryValue with
                | Some nextValue -> 
                    return! RecursiveRule (nextValue :: valueList) status
                | None ->
                    return valueList, status
            }
        Classifiers.sub continuation {
            let! (values, status) = RecursiveRule [] status
            return List.rev values, status
        }
            
    /// Replacement for single case discriminated union.
    let ZeroOrMoreConsumer<'a,'c> (classifier: ClassifierStatus<'a> -> ClassifierResult<'a>) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, string list, 'c>) =
        ZeroOrMore (classifierToBuilderFunction classifier) status continuation
        
    /// Discards given classifier before the function
    let WithDiscardBefore discardFunction function1 status continuation =
        Classifiers.sub continuation {
            let! status = Classifier.discard discardFunction status
            let! (variable, status) = function1 status
            return variable, status
        }

    /// Discards given classifier after the function
    let WithDiscardAfter discardFunction function1 status continuation =
        Classifiers.sub continuation {
            let! (variable, status) = function1 status        
            let! status = Classifier.discard discardFunction status
            return variable, status
        }
        
    /// Maps a consumer's text to a value
    let MapConsumer mapper classifier status continuation =
        Classifiers.sub continuation {
            let! (status: ClassifierStatus<_>) = classifier status
            let value = mapper status.ConsumedText
            return value, status
        }
        
    /// Names a consumer as a given value
    let NameConsumer name classifier status continuation =
        Classifiers.sub continuation {
            let! (status: ClassifierStatus<_>) = classifier status
            return name, status
        }
        
    /// Maps a classifier's value to a value    
    let MapClassifier mapper classifier status continuation =
        Classifiers.sub continuation {
            let! (value, status) = classifier status
            return (mapper value), status
        }
