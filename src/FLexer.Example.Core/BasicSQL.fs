module FLexer.Example.Core.BasicSQL

open FLexer.Core
open FLexer.Core.Tokenizer


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
    Classifiers.sub continuation {
        let! status = Classifier.map TokenType.ColumnName IDENTIFIER status
        let columnName = status.ConsumedText
        return SQLQueryColumn.Column(columnName), status
    }

let AcceptColumnNameWithTableName status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.map TokenType.TableName IDENTIFIER status
        let tableName = status.ConsumedText
        let! status = Classifier.discard PERIOD status
        let! status = Classifier.map TokenType.ColumnName IDENTIFIER status
        let columnName = status.ConsumedText
        return SQLQueryColumn.ColumnWithTableName(columnName, tableName), status
    }

let AcceptAllColumnTypes status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard COMMA status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        
        let! (value, status) = ClassifierFunction.PickOne [ AcceptColumnNameWithTableName; AcceptColumnName ] status
        return value, status
    }


let AcceptSQLQuery status =
    Classifiers.root() {
        // Add to token list, but don't return TokenType
        let! status = Classifier.name TokenType.Select SELECT status
        let! status = Classifier.discard WHITESPACE status
        
        // Add to token list, and return list of TokenTypes. Uses above parsing expression
        let! (column1, status) = ClassifierFunction.PickOne [ AcceptColumnName; AcceptColumnNameWithTableName ] status
        let! (moreColumns, status) = ClassifierFunction.ZeroOrMore AcceptAllColumnTypes status
        let allColumns = column1 :: moreColumns

        // Ignore whitespace
        let! status = Classifier.discard WHITESPACE status
        let! status = Classifier.name TokenType.From FROM status
        let! status = Classifier.discard WHITESPACE status

        // Deconstruct the returned TokenType
        let! status = Classifier.map TokenType.TableName IDENTIFIER status
        let tableName = status.ConsumedText


        // Return the resulting of this parsing expression.
        return {
            SQLQuery.Columns = allColumns
            SQLQuery.Table = tableName
        }, status
    }

let ExampleTester = ClassifierStatus<string>.OfString >> AcceptSQLQuery

/// True if the string should be accepted, false if should be rejected.
let ExampleStrings =
    [   true, "SELECT  LastName, FirstName, ID  , BirthDay  FROM Contacts"
        false, "SELECT Column1, Column2,,,NoColumn FROM Contacts"
        true, "SELECT  Contacts.LastName, FirstName, Contacts.ID  , BirthDay  FROM Contacts"
        true, "SELECT  LastName  FROM Contacts"
        true, "SELECT  Contacts.LastName  FROM Contacts"
        true, "SELECT  LastName , Contacts.FirstName FROM Contacts"
    ]

let Example() =
    ExampleStrings
    |> List.iter(fun (_, stringToTest) ->
        stringToTest
        |> ExampleTester
        |> (FLexer.Example.Core.Utility.PrintBuilderResults (printfn "%A") stringToTest)
    )
