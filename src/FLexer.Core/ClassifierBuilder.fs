namespace FLexer.Core

open Tokenizer


/// The result of a ClassifierBuilder computation expression
type ClassifierBuilderResult<'a,'b> = Result<'b * ClassifierStatus<'a>, ClassifierError<'a>>

/// Given a ClassifierStatus, the computation expression returns a ClassifierBuilderResult
type ClassifierBuilderFunction<'a,'b,'c> = 'b * ClassifierStatus<'a> -> ClassifierBuilderResult<'a, 'c>

type RawClassifierBuilderContinuation<'a,'b,'c,'d> = ClassifierStatus<'a> -> ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'b>
type ClassifierBuilderContinuation<'a,'b,'c,'d> = ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'b>


module ClassifierBuilder =

    type PickOne<'a,'b,'c,'d> =
        | PickOne of Status: ClassifierStatus<'a> * Classifiers: RawClassifierBuilderContinuation<'a,'b,'c,'d> list

    type Multiple<'a,'b,'c,'d> =
        | ZeroOrOne of Status: ClassifierStatus<'a> * Classifier: RawClassifierBuilderContinuation<'a,'b,'c,'d>
        | ZeroOrMore of ClassifierBuilderContinuation<'a,'b,'c,'d>
        | OneOrMore of ClassifierBuilderContinuation<'a,'b,'c,'d>


module ClassifierBuilderResult =
    let mapValue mapper (value: 'b, status: ClassifierStatus<'a>) =
        (mapper value, status)

    let map mapper (result: ClassifierBuilderResult<_,_>): ClassifierBuilderResult<_,_> =
        result
        |> Result.map(fun (b, status) -> mapper b, status)

    let continueError (error: ClassifierBuilderResult<_,_>): ClassifierBuilderResult<_,_> =
        match error with
        | Ok _ -> failwith "Expected Error"
        | Error err -> Error err

    let bind (binder: ClassifierBuilderFunction<'a, 'b, 'c>) (result: ClassifierBuilderResult<'a,'b>): ClassifierBuilderResult<'a,'c> =
        result 
        |> Result.bind binder

    let defaultIfError (defaultValue: unit -> ClassifierBuilderResult<'a,'b>) (result: ClassifierBuilderResult<'a,'b>): ClassifierBuilderResult<'a,'b> =
        match result with
        | Ok _ -> result
        | Error _ -> defaultValue() 


type SubClassifierBuilder<'a,'b,'c>(continuation: ClassifierBuilderFunction<'a, 'b, 'c> ) =

    member this.Bind(classifierResult: ClassifierResult<'a>, f: ClassifierStatus<'a> -> ClassifierBuilderResult<'a,'b>): ClassifierBuilderResult<'a, 'c> =
        classifierResult
        |> Result.bind f
        |> ClassifierBuilderResult.bind continuation

    member this.Bind<'d>(classifier: ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'b>, f: ClassifierBuilderFunction<'a, 'd, 'c>): ClassifierBuilderResult<'a,'c> =
        f
        |> classifier
        |> ClassifierBuilderResult.bind continuation

    member this.Bind<'d>(ClassifierBuilder.PickOne(status: ClassifierStatus<'a>, classifiers: (ClassifierStatus<'a> -> ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'b>) list), f: ClassifierBuilderFunction<'a, 'd, 'c>): ClassifierBuilderResult<'a,'c> =

        let rec tryClassifier remainingClassifiers =
            match remainingClassifiers with
            | [] -> ClassifierBuilderResult.Error (ClassifierError<'a>.OfTokenizerError status (Some TokenizerError.LookaheadFailure))
            | classifier :: tail ->
                match classifier status f with
                | Ok _ as x -> x |> Result.bind continuation
                | Error _ ->
                    tryClassifier tail            
        tryClassifier classifiers        
                
    member this.Return(value, status): ClassifierBuilderResult<'a,'c> =
        ClassifierBuilderResult.Ok (value, status)
        

type RootClassifierBuilder<'a,'c>() =

    member this.Bind(classifierResult: ClassifierResult<'a>, f: ClassifierStatus<'a> -> ClassifierBuilderResult<'a,'c>): ClassifierBuilderResult<'a, 'c> =
        classifierResult
        |> Result.bind f

    member this.Bind<'d>(classifier: ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'c>, f: ClassifierBuilderFunction<'a, 'd, 'c>): ClassifierBuilderResult<'a,'c> =
        f
        |> classifier

        
    member this.Bind<'d>(ClassifierBuilder.PickOne(status: ClassifierStatus<'a>, classifiers: (ClassifierStatus<'a> -> ClassifierBuilderFunction<'a, 'd, 'c> -> ClassifierBuilderResult<'a, 'c>) list), f: ClassifierBuilderFunction<'a, 'd, 'c>): ClassifierBuilderResult<'a,'c> =
        let rec tryClassifier remainingClassifiers =
            match remainingClassifiers with
            | [] -> ClassifierBuilderResult.Error (ClassifierError<'a>.OfTokenizerError status (Some TokenizerError.LookaheadFailure))
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
        | _ ->
            failwith "what what"

        
    member this.Return(value, state): ClassifierBuilderResult<'a,'c> =
        ClassifierBuilderResult.Ok (value, state)


