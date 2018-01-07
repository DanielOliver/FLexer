open FLexer.Core
open FLexer.Core.Tokenizer


/// ######  Lexer words & regex  ######
let SELECT = Consumers.TakeWord "SELECT" true
let FROM = Consumers.TakeWord "FROM" true
let WHITESPACE = Consumers.TakeRegex "\s+" 
let OPTIONAL_WHITESPACE = Consumers.TakeRegex "\s*"
let IDENTIFIER = Consumers.TakeRegex "[A-Za-z][A-Za-z0-9]*"


/// ######  Parser Identifiers  ######
type TokenType = 
    | Select
    | ColumnName of string
    | From
    | TableName of string

/// ######  Output of parsing  ######
type SQLQuery =
    {   Columns: string list
        Table: string
    }

/// ######  Parser Functions  ######
let AcceptColumnName status =
    ClassifierBuilder status {
        do! Discard OPTIONAL_WHITESPACE
        let! tokenType = 
            Choice [
                Classifier.name TokenType.From FROM /// This will be taken first if available, thus stopping consumption of Identifiers.
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
        // Add to token list, but don't return TokenType
        do! Accept(Classifier.name TokenType.Select SELECT)
        
        // Add to token list, and return list of TokenTypes. Uses above parsing expression
        let! columns = OneOrMore AcceptColumnName

        // Ignore whitespace
        do! Discard WHITESPACE        
        do! Accept(Classifier.name TokenType.From FROM)
        do! Discard WHITESPACE

        // Deconstruct the returned TokenType
        let! (TokenType.TableName tableName) = Classifier.map TokenType.TableName IDENTIFIER

        // Return the resulting of this parsing expression.
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
            printfn "Accepted \"%s\"" stringToAccept
            printfn ""
            printfn "Query - %A" value
            printfn ""
            printfn "Status - %A" status
        | Error err -> printfn "%A" err)
    
    0 // return an integer exit code
