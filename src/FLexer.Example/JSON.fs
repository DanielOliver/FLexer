module JSON


open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Core.Classifier
open FLexer.Core.ClassifierBuilder


/// ######  Lexer words & regex  ######
let DoubleQuote = Consumers.TakeChar '"'
let LeftBracket = Consumers.TakeChar '['
let RightBracket = Consumers.TakeChar ']'
let Colon = Consumers.TakeChar ':'
let Comma = Consumers.TakeChar ','

let NULL = Consumers.TakeWord "null" false
let TRUE = Consumers.TakeWord "true" false
let FALSE = Consumers.TakeWord "false" false
let StringLiteral = Consumers.TakeRegex "([\\][\"]|.)*"
let OPTIONAL_WHITESPACE = Consumers.TakeRegex "(\s|[\r\n])*"
let NumberRegex = Consumers.TakeRegex "([-]){0,1}[0-9]+([.]([0-9])+){0,1}(e(+|-){0,1}(0-9)+)"

/// https://github.com/fsharp/FSharp.Data/blob/f000cc1a9ba19e5187b5828acbdc66a701142eb4/src/Json/JsonValue.fs#L34-L41
[<RequireQualifiedAccessAttribute>]
type JsonValue =
  | String of string
  | Number of decimal
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
        let! status = Classifier.map (System.Convert.ToDecimal >> JsonValue.Number) NumberRegex status
        return status.Classification, status
    }
    
and AcceptStringLiteral status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard DoubleQuote status
        let! status = Classifier.map (JsonValue.String) StringLiteral status
        let! status = Classifier.discard DoubleQuote status
        return status.Classification, status
    }

and AcceptArray status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! (arrayValues, status) = ClassifierBuilder.ZeroOrMore(status, AcceptJsonObject)
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        return arrayValues, status
    }

and AcceptJsonKeyPair status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        
        let! (keyName, status) = AcceptStringLiteral status
        let keyNameText = match keyName with | JsonValue.String str -> str | _ -> failwith "need better error"

        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! (value, status) = AcceptJsonObject status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        return (keyNameText, value), status
    }

and AcceptJsonKeyPairWithComma status (continuation: ClassifierBuilderFunction<JsonValue, string * JsonValue, JsonValue>): ClassifierBuilderResult<JsonValue, JsonValue> =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard Comma status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! (value, status) = AcceptJsonKeyPair status
        return value, status
    }

and AcceptJsonRecord status continuation =
    Classifiers.sub continuation {

        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard LeftBracket status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! (tryFirstItem, status) = ZeroOrOne(status, AcceptJsonKeyPair)
        
        if tryFirstItem.IsEmpty then
            let! status = Classifier.discard OPTIONAL_WHITESPACE status
            let! status = Classifier.discard RightBracket status
            let! status = Classifier.discard OPTIONAL_WHITESPACE status

            return (JsonValue.Record Array.empty), status
        else
            let firstItem = tryFirstItem.Head
            //let! (recordList, status) = ZeroOrMore(status, AcceptJsonKeyPairWithComma)
            //let value = (firstItem :: (List.rev recordList)) |> Array.ofList
            let value = firstItem |> List.singleton |> Array.ofList


            let! status = Classifier.discard OPTIONAL_WHITESPACE status
            let! status = Classifier.discard RightBracket status
            let! status = Classifier.discard OPTIONAL_WHITESPACE status
            return (JsonValue.Record Array.empty), status
    }

and AcceptJsonObject status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! (value, status) = PickOne(status, [ AcceptNull; AcceptNumber; AcceptStringLiteral; AcceptBooleanFalse; AcceptBooleanTrue; AcceptJsonRecord ])
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        return value, status
    }
//and AcceptArray status continuation =
//    Classifiers.sub continuation {
//        let! status = Classifier.discard LeftBracket status
//        let! status = Classifier.name (JsonValue.String) StringLiteral status

//        let firstValue 

//        let! status = Classifier.discard RightBracket status
//        return status.Classification, status
//    }
