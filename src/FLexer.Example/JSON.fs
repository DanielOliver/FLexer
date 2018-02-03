module FLexer.Example.JSON


open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Core.Classifier
open FLexer.Core.ClassifierBuilder


/// ######  Lexer words & regex  ######
let DoubleQuote = Consumers.TakeChar '"'
let LeftBracket = Consumers.TakeChar '['
let RightBracket = Consumers.TakeChar ']'
let CurlyLeftBracket = Consumers.TakeChar '{'
let CurlyRightBracket = Consumers.TakeChar '}'
let Colon = Consumers.TakeChar ':'
let Comma = Consumers.TakeChar ','

let NULL = Consumers.TakeWord "null" false
let TRUE = Consumers.TakeWord "true" false
let FALSE = Consumers.TakeWord "false" false
let StringLiteral = Consumers.TakeRegex "([\\\\][\"]|[^\"])*"
let OPTIONAL_WHITESPACE = Consumers.TakeRegex @"(\s|[\r\n])*"
let NumberRegex = Consumers.TakeRegex "([-]){0,1}[0-9]+([.]([0-9])+){0,1}((e|E)([+]|[-]){0,1}[0-9]+){0,1}"



/// https://github.com/fsharp/FSharp.Data/blob/f000cc1a9ba19e5187b5828acbdc66a701142eb4/src/Json/JsonValue.fs#L34-L41
/// ######  Output of parsing  ######
[<RequireQualifiedAccessAttribute>]
type JsonValue =
  | String of string
  | Number of string
  | Record of properties:(string * JsonValue)[]
  | Array of elements:JsonValue[]
  | Boolean of bool
  | Null  

[<RequireQualifiedAccessAttribute>]
type JsonValueType =
  | String 
  | Number 
  | Record 
  | Array 
  | Boolean
  | Null

    static member Of (value: JsonValue) =
        match value with
        | JsonValue.String _ -> JsonValueType.String
        | JsonValue.Number _ -> JsonValueType.Number
        | JsonValue.Record _ -> JsonValueType.Record
        | JsonValue.Array _ -> JsonValueType.Array
        | JsonValue.Boolean _ -> JsonValueType.Boolean
        | JsonValue.Null -> JsonValueType.Null


  
/// ######  Primitive Parser Functions  ######
let rec AcceptBooleanTrue status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name (JsonValue.Boolean true) TRUE status
        return status.Classification, status
    }
    
and AcceptBooleanFalse status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name (JsonValue.Boolean false) FALSE status
        return status.Classification, status
    }
    
and AcceptNull status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name (JsonValue.Null) NULL status
        return status.Classification, status
    }
    
and AcceptNumber status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.map (JsonValue.Number) NumberRegex status
        return status.Classification, status
    }
    
and AcceptStringLiteral status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard DoubleQuote status
        let! status = Classifier.map (JsonValue.String) StringLiteral status
        let! status = Classifier.discard DoubleQuote status
        return status.Classification, status
    }



/// ######  Recursive Parser Functions  ######
let rec AcceptArrayElement arrayType elements originalStatus continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE originalStatus

        if List.isEmpty elements then
            //let! (value, status) = AcceptJsonObject status
            let! (tryFirstValue, status) = ClassifierBuilder.ZeroOrOne(status, AcceptJsonObject)
            match tryFirstValue with
            | None -> 
                return [], status
            | Some firstValue ->
                return! AcceptArrayElement (JsonValueType.Of firstValue) [ firstValue ] status
        else
            match Classifier.discard Comma status with 
            | ClassifierResult.Ok(status) -> 
                let! status = Classifier.discard OPTIONAL_WHITESPACE status

                let! (value, status) = AcceptJsonObject status

                if (JsonValueType.Of value) = arrayType then
                    return! AcceptArrayElement (JsonValueType.Of value) (value :: elements) status
                else
                    return! status

            | ClassifierResult.Error _ -> 
                return elements, status
    }

and AcceptFirstArrayElement status continuation = 
    AcceptArrayElement JsonValueType.Null [] status continuation

and AcceptArray status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard LeftBracket status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! (items, status) = AcceptFirstArrayElement status

        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard RightBracket status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let finalValues = items |> Seq.rev |> Array.ofSeq
        return (JsonValue.Array finalValues), status
    }

and AcceptJsonKeyPair status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        
        let! (keyName, status) = AcceptStringLiteral status
        let keyNameText = match keyName with | JsonValue.String str -> str | _ -> failwith "need better error"

        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard Colon status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! (value, status) = AcceptJsonObject status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        return (keyNameText, value), status
    }

and AcceptJsonKeyPairWithComma jsonKeyPairs status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let result = Classifier.discard Comma status
        match result with 
        | ClassifierResult.Ok(status) -> 
            let! status = Classifier.discard OPTIONAL_WHITESPACE status
            let! (value, status) = AcceptJsonKeyPair status
            return (value :: jsonKeyPairs), status
        | ClassifierResult.Error _ -> 
            return jsonKeyPairs, status
    }

and AcceptJsonRecord status continuation =
    Classifiers.sub continuation {

        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard CurlyLeftBracket status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! (tryFirstItem, status) = ZeroOrOne(status, AcceptJsonKeyPair)
        

        match tryFirstItem with
        | None ->
            let! status = Classifier.discard OPTIONAL_WHITESPACE status
            let! status = Classifier.discard CurlyRightBracket status
            let! status = Classifier.discard OPTIONAL_WHITESPACE status

            return (JsonValue.Record Array.empty), status

        | Some(firstItem) ->
            let! (allItems, status) = AcceptJsonKeyPairWithComma [ firstItem ] status
            let finalValue = allItems |> List.rev |> Array.ofList

            let! status = Classifier.discard OPTIONAL_WHITESPACE status
            let! status = Classifier.discard CurlyRightBracket status
            let! status = Classifier.discard OPTIONAL_WHITESPACE status
            return (JsonValue.Record finalValue), status
    }

and AcceptJsonObject status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! (value, status) = PickOne(status, [ AcceptNull; AcceptNumber; AcceptStringLiteral; AcceptBooleanFalse; AcceptBooleanTrue; AcceptJsonRecord; AcceptArray ])
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        return value, status
    }

let AcceptJson status: ClassifierBuilderResult<JsonValue, JsonValue> =
    Classifiers.root() {
        let! (jsonObject, status) = AcceptJsonObject status
        return jsonObject, status
    }


let ExampleTester = ClassifierStatus<string>.OfString >> AcceptJson

/// True if the string should be accepted, false if should be rejected.
let ExampleStrings =
    [   true, "{ }"
        true, """{ "valueOne": "valueTwo" }"""
        false, "{ -234.0: [] }"
        true, """{ "valueOne": -123.450 }"""
        true, """{ "valueOne": "valueTwo"  ,"value  Three "  : { "a key": 345 } }"""
        true, " [ ] "
        true, " [ 23498, 09, 45, 123.0e+10, -12e-499, 9.09834E-23 ] "
        false, " [ {}, 09 ] "
        true, """ [ {}, { "key2": { "subObject": [] } }, { "items": [ 7, 8, 9 ] } ] """
    ]

let Example() =
    ExampleStrings
    |> List.iter(fun (_, stringToTest) ->
        stringToTest
        |> ExampleTester
        |> (FLexer.Example.Utility.PrintBuilderResults (printfn "%A") stringToTest)
    )    
