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
        
/// Keywords used in ClassifierBuilder expressions
module ClassifierBuilder =
    
    type ZeroOrOne<'a,'b,'c,'d> =
        | ZeroOrOne of Status: ClassifierStatus<'a> * Classifier: ClassifierBuilderContinuationFromStatus<'a,'b,'c,'d>

    type ZeroOrMore<'a,'b,'c,'d> =
        | ZeroOrMore of Status: ClassifierStatus<'a> * Classifier: ClassifierBuilderContinuationFromStatus<'a,'b,'c,'d>
    
    type OneOrMore<'a,'b,'c,'d> =
        | OneOrMore of Status: ClassifierStatus<'a> * Classifier: ClassifierBuilderContinuationFromStatus<'a,'b,'c,'d>

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
    let PickOneConsumer<'a,'c,'d> (classifiers: (ClassifierStatus<'a> -> ClassifierResult<'a>) list) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, string, 'c>) =
        PickOne (classifiers |> List.map classifierToBuilderFunction)



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


    member this.Bind<'d>(ClassifierBuilder.ZeroOrOne(status: ClassifierStatus<'a>, classifier: ClassifierBuilderContinuationFromStatus<'a, 'c, 'c, 'd>), f: ClassifierBuilderFunction<'a, 'd option, 'c>): ClassifierBuilderResult<'a,'c> =
        match classifier status (ClassifierBuilderResult.mapValue (Some) >> f) with
        | Ok _ as x -> x
        | Error _ -> (None, status) |> f
        
    
    member this.Bind<'d>(ClassifierBuilder.ZeroOrMore(status: ClassifierStatus<'a>, classifier: ClassifierBuilderContinuationFromStatus<'a, 'd, 'd, 'd>), f: ClassifierBuilderFunction<'a, 'd list, 'c>): ClassifierBuilderResult<'a,'c> =
        let rec acceptColumns nextStatus statusList (valueList: 'd list): (ClassifierStatus<'a> list * 'd list) =
            match classifier nextStatus Continuation.None with
            | Ok (value, newStatus) -> acceptColumns newStatus (newStatus :: statusList) (value :: valueList)
            | Error _ -> statusList, valueList

        let (statusList: ClassifierStatus<'a> list, valueList: 'd list) = acceptColumns status [] []

        let rec testStatus statusList valueList: ClassifierBuilderResult<'a,'c> =
            match statusList, valueList with
            | (currentStatus :: statusTail), (_ :: valueTail) ->
                (valueList, currentStatus)
                |> f
                |> ClassifierBuilderResult.defaultIfError(
                    fun () -> testStatus statusTail valueTail)
            | _, _ -> f([], status)
                    
        testStatus statusList valueList


    member this.Bind<'d>(ClassifierBuilder.OneOrMore(status: ClassifierStatus<'a>, classifier: ClassifierBuilderContinuationFromStatus<'a, 'd, 'd, 'd>), f: ClassifierBuilderFunction<'a, 'd list, 'c>): ClassifierBuilderResult<'a,'c> =
        let rec acceptColumns nextStatus statusList (valueList: 'd list) =
            match classifier nextStatus (ClassifierBuilderResult.Ok) with
            | Ok (value, newStatus) -> acceptColumns newStatus (newStatus :: statusList) (value :: valueList)
            | Error _ -> statusList, valueList

        let (statusList: ClassifierStatus<'a> list, valueList: 'd list) = acceptColumns status [] []

        let rec testStatus statusList valueList: ClassifierBuilderResult<'a,'c> =
            match statusList, valueList with
            | (currentStatus :: statusTail), (_ :: valueTail) ->
                (valueList, currentStatus)
                |> f
                |> ClassifierBuilderResult.defaultIfError(
                    fun () -> testStatus statusTail valueTail)
            | _, _ -> ClassifierBuilderResult.Error (ClassifierError<'a>.OfTokenizerError status (Some Tokenizer.TokenizerError.LookaheadFailure))
                    
        testStatus statusList valueList


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
