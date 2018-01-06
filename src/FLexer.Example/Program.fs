open FLexer.Core
open FLexer.Core.Tokenizer

let SELECT = Consumers.TakeWord "SELECT" true

let FROM = Consumers.TakeWord "FROM" true

let WHITESPACE = Consumers.TakeRegex "\s+" 
let OPTIONAL_WHITESPACE = Consumers.TakeRegex "\s*"

let IDENTIFIER = Consumers.TakeRegex "[A-Za-z][A-Za-z0-9]*"


type TokenType = 
    | Select
    | ColumnName of string
    | From
    | TableName of string

type SQLQuery =
    {   Columns: string list
        Table: string
    }

let AcceptColumnName status =
    ClassifierBuilder status {
        do! Discard OPTIONAL_WHITESPACE
        let! tokenType = 
            Choice [
                Classifier.name TokenType.From FROM
                Classifier.map TokenType.ColumnName IDENTIFIER
            ]
        match tokenType with
        | TokenType.ColumnName columnName ->
            return columnName        
        | _ ->
            return Failure
    }
        
let AcceptSQLQuery status =
    ClassifierBuilder status {
        do! Accept(Classifier.name TokenType.Select SELECT)
        
        let! columns = 
            OneOrMore AcceptColumnName

        do! Discard WHITESPACE        
        do! Accept(Classifier.name TokenType.From FROM)
        do! Discard WHITESPACE

        let! (TokenType.TableName tableName) = Classifier.map TokenType.TableName IDENTIFIER

        return {
            SQLQuery.Columns = columns
            SQLQuery.Table = tableName
        }
        
    }


[<EntryPoint>]
let main argv =

    let stringToAccept = "SELECT  Column1 FROM Table234"

    stringToAccept
    |> ClassifierStatus<string>.OfString 
    |> AcceptSQLQuery
    |> (function
        | Ok(value, status) -> 
            printfn "ACCEPTED \"%s\"" stringToAccept
            printfn ""
            printfn "Query - %A" value
            printfn ""
            printfn "Status - %A" status
        | Error err -> printfn "%A" err)
    
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
