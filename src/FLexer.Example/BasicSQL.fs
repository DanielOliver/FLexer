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
let OPTIONAL_WHITESPACE = Consumers.TakeRegex "(\s|[\r\n])*"
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
let AcceptColumnName status continuation =
    SubClassifierBuilder continuation {
        let! status = Classifier.map TokenType.ColumnName IDENTIFIER status
        let (ColumnName columnName) = status.Classification
        return SQLQueryColumn.Column(columnName), status
    }

let AcceptColumnNameWithTableName status continuation =
    SubClassifierBuilder continuation {
        let! status = Classifier.map TokenType.TableName IDENTIFIER status
        let (TableName tableName) = status.Classification
        let! status = Classifier.discard COMMA status
        let! status = Classifier.map TokenType.ColumnName IDENTIFIER status
        let (TableName columnName) = status.Classification
        return SQLQueryColumn.ColumnWithTableName(columnName, tableName), status
    }

let AcceptAllColumnTypes status continuation =
    SubClassifierBuilder continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard COMMA status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! result = ClassifierBuilder.PickOne(status, [ AcceptColumnNameWithTableName; AcceptColumnName ])
        return result
    }


let AcceptSQLQuery status =
    RootClassifierBuilder() {
        // Add to token list, but don't return TokenType
        let! status = Classifier.name TokenType.Select SELECT status
        let! status = Classifier.discard WHITESPACE status
        
        // Add to token list, and return list of TokenTypes. Uses above parsing expression
        let! (column1, status) = ClassifierBuilder.PickOne(status, [ AcceptColumnName; AcceptColumnNameWithTableName ])
        let! (moreColumns, status) = ClassifierBuilder.ZeroOrOne(status, AcceptAllColumnTypes)
        let allColumns = column1 :: (List.rev moreColumns)
        printfn "%A" allColumns

        // Ignore whitespace
        let! status = Classifier.discard WHITESPACE status
        let! status = Classifier.name TokenType.From FROM status
        let! status = Classifier.discard WHITESPACE status

        // Deconstruct the returned TokenType
        let! status = Classifier.map TokenType.TableName IDENTIFIER status
        let (TableName tableName) = status.Classification


        // Return the resulting of this parsing expression.
        return {
            SQLQuery.Columns = allColumns
            SQLQuery.Table = tableName
        }, status
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