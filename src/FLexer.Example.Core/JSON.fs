module FLexer.Example.Core.JSON


open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Core.Classifier


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
let StringLiteral = Consumers.TakeRegex "([\\\\][\"]|[^\"])+"
let WHITESPACE = Consumers.TakeRegex @"(\s|[\r\n])+"
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
let OPTIONAL_WHITESPACE status = ClassifierFunction.ZeroOrOneConsumer (Classifier.discard WHITESPACE) status

let rec AcceptBooleanTrue status continuation =
    Classifiers.sub continuation {
        let! status = name (JsonValue.Boolean true) TRUE status
        return status.Classification, status
    }
    
and AcceptBooleanFalse status continuation =
    Classifiers.sub continuation {
        let! status = name (JsonValue.Boolean false) FALSE status
        return status.Classification, status
    }
    
and AcceptNull status continuation =
    Classifiers.sub continuation {
        let! status = name (JsonValue.Null) NULL status
        return status.Classification, status
    }
    
and AcceptNumber status continuation =
    Classifiers.sub continuation {
        let! status = map (JsonValue.Number) NumberRegex status
        return status.Classification, status
    }
    
and AcceptStringLiteral status continuation =
    Classifiers.sub continuation {
        let! status = discard DoubleQuote status
        let! (literal, status) = ClassifierFunction.ZeroOrOneConsumer (map JsonValue.String StringLiteral) status
        let classification = if literal.IsSome then status.Classification else JsonValue.String ""
        let! status = discard DoubleQuote status
        return classification, status
    }



/// ######  Recursive Parser Functions  ######
let rec AcceptArrayElement arrayType elements originalStatus continuation =
    Classifiers.sub continuation {
        let! (_, status) = OPTIONAL_WHITESPACE originalStatus

        if List.isEmpty elements then
            let! (tryFirstValue, status) = ClassifierFunction.ZeroOrOne AcceptJsonObject status
            match tryFirstValue with
            | None -> 
                return [], status
            | Some firstValue ->
                return! AcceptArrayElement (JsonValueType.Of firstValue) [ firstValue ] status
        else
            match discard Comma status with 
            | Ok(status) -> 
                let! (_, status) = OPTIONAL_WHITESPACE status

                let! (value, status) = AcceptJsonObject status

                if (JsonValueType.Of value) = arrayType then
                    return! AcceptArrayElement (JsonValueType.Of value) (value :: elements) status
                else
                    return! status

            | Error _ -> 
                return elements, status
    }

and AcceptFirstArrayElement status continuation = 
    AcceptArrayElement JsonValueType.Null [] status continuation

and AcceptArray status continuation =
    Classifiers.sub continuation {
        
        let! (_, status) = OPTIONAL_WHITESPACE status
        let! status = discard LeftBracket status
        let! (_, status) = OPTIONAL_WHITESPACE status

        let! (items, status) = AcceptFirstArrayElement status

        
        let! (_, status) = OPTIONAL_WHITESPACE status
        let! status = discard RightBracket status
        let! (_, status) = OPTIONAL_WHITESPACE status

        let finalValues = items |> Seq.rev |> Array.ofSeq
        return (JsonValue.Array finalValues), status
    }

and AcceptJsonKeyPair status continuation =
    Classifiers.sub continuation {
        let! (_, status) = OPTIONAL_WHITESPACE status
        
        let! (keyName, status) = AcceptStringLiteral status
        let keyNameText = match keyName with | JsonValue.String str -> str | _ -> failwith "Should never reach this."
        
        
        let! (_, status) = OPTIONAL_WHITESPACE status
        let! status = discard Colon status
        let! (_, status) = OPTIONAL_WHITESPACE status

        let! (value, status) = AcceptJsonObject status
        let! (_, status) = OPTIONAL_WHITESPACE status
        return (keyNameText, value), status
    }

and AcceptRecordKeyPairs elements status continuation =
    Classifiers.sub continuation {
        let! (_, status) = OPTIONAL_WHITESPACE status

        if List.isEmpty elements then
            let! (tryFirstValue, status) = ClassifierFunction.ZeroOrOne AcceptJsonKeyPair status
            match tryFirstValue with
            | None -> 
                return [], status
            | Some firstValue ->
                return! AcceptRecordKeyPairs [ firstValue ] status
        else
            let! (_, status) = OPTIONAL_WHITESPACE status
            let result = discard Comma status
            match result with 
            | Ok(status) -> 
                let! (_, status) = OPTIONAL_WHITESPACE status

                let! (value, status) = AcceptJsonKeyPair status
                return! AcceptRecordKeyPairs (value :: elements) status

            | Error _ -> 
                return elements, status
    }

and AcceptJsonRecord status continuation =
    Classifiers.sub continuation {
        
        let! (_, status) = OPTIONAL_WHITESPACE status
        let! status = discard CurlyLeftBracket status
        let! (_, status) = OPTIONAL_WHITESPACE status
                
        let! (items, status) = AcceptRecordKeyPairs [] status
        let finalValue = items |> List.rev |> Array.ofList
        

        let! (_, status) = OPTIONAL_WHITESPACE status
        let! status = discard CurlyRightBracket status
        let! (_, status) = OPTIONAL_WHITESPACE status
        
        
        return (JsonValue.Record finalValue), status        
    }

and AcceptJsonObject status continuation =
    Classifiers.sub continuation {
        
        let! (_, status) = OPTIONAL_WHITESPACE status
        let! (value, status) = ClassifierFunction.PickOne [ AcceptNull; AcceptNumber; AcceptStringLiteral; AcceptBooleanFalse; AcceptBooleanTrue; AcceptJsonRecord; AcceptArray ] status
        let! (_, status) = OPTIONAL_WHITESPACE status

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
        true, """
{
    "glossary": {
        "title": "example glossary",
        "GlossDiv": {
            "title": "S",
            "GlossList": {
                "GlossEntry": {
                    "ID": "SGML",
                    "SortAs": "SGML",
                    "GlossTerm": "Standard Generalized Markup Language",
                    "Acronym": "SGML",
                    "Abbrev": "ISO 8879:1986",
                    "GlossDef": {
                        "para": "A meta-markup language, used to create markup languages such as DocBook.",
                        "GlossSeeAlso": ["GML", "XML"]
                    },
                    "GlossSee": "markup"
                }
            }
        }
    }
} """
        true, """
{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": { 
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}"""
    ]

let Example() =
    ExampleStrings
    |> List.iter(fun (_, stringToTest) ->
        stringToTest
        |> ExampleTester
        |> (FLexer.Example.Core.Utility.PrintBuilderResults (printfn "%A") stringToTest)
    )
