﻿namespace FLexer.Core

open FLexer.Core

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

    type PickOne<'a,'b,'c,'d> =
        | PickOne of Status: ClassifierStatus<'a> * Classifiers: ClassifierBuilderContinuationFromStatus<'a,'b,'c,'d> list

    type Multiple<'a,'b,'c,'d> =
        | ZeroOrOne of Status: ClassifierStatus<'a> * Classifier: ClassifierBuilderContinuationFromStatus<'a,'b,'c,'d>
        | ZeroOrMore of Status: ClassifierStatus<'a> * Classifier: ClassifierBuilderContinuationFromStatus<'a,'d,'d,'d>
        | OneOrMore of Status: ClassifierStatus<'a> * Classifier: ClassifierBuilderContinuationFromStatus<'a,'d,'d,'d>
        

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


/// A computation expression to compose functions with
type SubClassifierBuilder<'a,'b,'c>(continuation: ClassifierBuilderFunction<'a, 'b, 'c> ) =

    member this.Bind(classifierResult: ClassifierResult<'a>, f: ClassifierStatus<'a> -> ClassifierBuilderResult<'a,'c>): ClassifierBuilderResult<'a, 'c> =
        classifierResult
        |> Result.bind f


    member this.Bind<'d>(classifier: ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'c>, f: ClassifierBuilderFunction<'a, 'd, 'c>): ClassifierBuilderResult<'a,'c> =
        f
        |> classifier


    member this.Bind<'d>(ClassifierBuilder.PickOne(status: ClassifierStatus<'a>, classifiers: ClassifierBuilderContinuationFromStatus<'a,'c,'c,'d> list), f: ClassifierBuilderFunction<'a, 'd, 'c>): ClassifierBuilderResult<'a,'c> =
        let rec tryClassifier remainingClassifiers =
            match remainingClassifiers with
            | [] -> ClassifierBuilderResult.Error (ClassifierError<'a>.OfTokenizerError status (Some Tokenizer.TokenizerError.LookaheadFailure))
            | classifier :: tail ->
                match classifier status f with
                | Ok _ as x -> x
                | Error _ ->
                    tryClassifier tail
        tryClassifier classifiers

        
    member this.Bind<'d>(multipleClassifier: ClassifierBuilder.Multiple<'a,'c,'c,'d>, f: ClassifierBuilderFunction<'a, 'd list, 'c>): ClassifierBuilderResult<'a,'c> =
        match multipleClassifier with
        | ClassifierBuilder.Multiple.ZeroOrOne (status, classifier) ->
            match classifier status (ClassifierBuilderResult.mapValue (List.singleton) >> f) with
            | Ok _ as x -> x
            | Error _ -> (List.empty, status) |> f

        | ClassifierBuilder.Multiple.ZeroOrMore (status, classifier) ->
            let rec acceptColumns nextStatus statusList (valueList: 'd list) =
                match classifier nextStatus Ok with
                | Ok (value, newStatus) as x -> acceptColumns newStatus (newStatus :: statusList) (value :: valueList)
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


        | ClassifierBuilder.Multiple.OneOrMore (status, classifier) ->
            let rec acceptColumns nextStatus statusList (valueList: 'd list) =
                match classifier nextStatus Ok with
                | Ok (value, newStatus) as x -> acceptColumns newStatus (newStatus :: statusList) (value :: valueList)
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

    
module Classifiers = 
    let root() = SubClassifierBuilder(Ok)
    let sub continuation = SubClassifierBuilder(continuation)
