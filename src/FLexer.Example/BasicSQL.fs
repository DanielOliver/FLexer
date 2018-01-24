module FLexer.Example.BasicSQL

open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Core


/// ######  Lexer words & regex  ######
let SELECT = Consumers.TakeWord "SELECT" true
let FROM = Consumers.TakeWord "FROM" true
let WHITESPACE = Consumers.TakeRegex "(\s|[\r\n])+"
let COMMA = Consumers.TakeChar ','
let PERIOD = Consumers.TakeChar '.'
let OPTIONAL_WHITESPACE = Consumers.TakeRegex "\s*"
let IDENTIFIER = Consumers.TakeRegex "[A-Za-z][A-Za-z0-9]*"


/// ######  Parser Identifiers  ######
type TokenType = 
    | Select
    | ColumnName of string
    | ColumnIdentifier of string
    | From
    | TableName of string

type SQLQueryColumn =
    | Column of string
    | ColumnWithTableName of ColumnName: string * TableName: string

/// ######  Output of parsing  ######
type SQLQuery =
    {   Columns: SQLQueryColumn list
        Table: string
    }



/// ######  Parser Functions  ######
let AcceptColumnName status =
    ClassifierBuilder status {
        let! (TokenType.ColumnName columnName) = Classifier.map TokenType.ColumnName IDENTIFIER
        return SQLQueryColumn.Column columnName
    }

let AcceptColumnNameWithTableName status =
    ClassifierBuilder status {
        let! (TokenType.TableName tableName) = Classifier.map TokenType.TableName IDENTIFIER
        do! Discard PERIOD
        let! (TokenType.ColumnName columnName) = Classifier.map TokenType.ColumnName IDENTIFIER
        return SQLQueryColumn.ColumnWithTableName(columnName, tableName)
    }

let AcceptAllColumnTypes status =
    ClassifierBuilder status {
        do! Discard OPTIONAL_WHITESPACE
        do! Discard COMMA
        do! Discard OPTIONAL_WHITESPACE

        let! status = [ AcceptColumnName; AcceptColumnNameWithTableName ]
        return status
    }

let AcceptSQLQuery status =
    ClassifierBuilder status {
        // Add to token list, but don't return TokenType
        do! Accept(Classifier.name TokenType.Select SELECT)
        do! Discard WHITESPACE
        
        // Add to token list, and return list of TokenTypes. Uses above parsing expression
        let! column1 = [ AcceptColumnName; AcceptColumnNameWithTableName ]
        let! moreColumns = ZeroOrMore AcceptAllColumnTypes
        let allColumns = column1 :: (List.rev moreColumns)

        // Ignore whitespace
        do! Discard WHITESPACE        
        do! Accept(Classifier.name TokenType.From FROM)
        do! Discard WHITESPACE

        // Deconstruct the returned TokenType
        let! (TokenType.TableName tableName) = Classifier.map TokenType.TableName IDENTIFIER

        // Return the resulting of this parsing expression.
        return {
            SQLQuery.Columns = allColumns
            SQLQuery.Table = tableName
        }        
    }

let Example() = 
    let stringsToAccept =
        [   "SELECT  LastName, FirstName, ID  , BirthDay  FROM Contacts"
            "SELECT Column1, Column2,,,NoColumn FROM Contacts"
            "SELECT  Contacts.LastName, FirstName, Contacts.ID  , BirthDay  FROM Contacts"
        ]

    stringsToAccept
    |> List.iter(fun stringToTest ->
        stringToTest
        |> ClassifierStatus<string>.OfString 
        |> AcceptSQLQuery
        |> (FLexer.Example.Utility.PrintBuilderResults (printfn "%A") stringToTest)
    )    
