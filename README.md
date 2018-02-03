# FLexer

### FLexer is a F# Lexer and Parser dedicated to ease of use and expressiveness in creating custom recursive-descent parsers.

## Description

FLexer is a library combining together a Lexer and Parser, to allow ease of construction in custom recursive-descent parsers with infinite backtrack. When searching for a result to "_accept_", FLexer functions as a depth first search in completely traversing any path, before backtracking and trying a different route.

FLexer is NOT a parser generator. Rather, it's up the user to write the code for the parser. This puts the responsibility of writing a performance optimal parser on the developer. The happy path of parsing will be "right first try" and very fast; however, the potential to backtrack to the worst case still exists.

**Reasons to use FLexer**

* Each component of the recursive descent parser may be tested in isolation, after all, it's just functions.
* It's simple to get started. There's no parser generator or external tools to integrate into the build pipeline, it's just F#.
* Cross-platform with the core written in .NET Standard 2.0 and no dependencies.
* Complete customization of the parser is available at any step, such as inserting validation code inline with the parser or even using parsed data to change the parser's functionality.

**Reasons to NOT use FLexer**

* Parser generators can suffice for most requirements.

## Build

| Appveyor | Nuget |
|---:|:---|
| [![Build status](https://ci.appveyor.com/api/projects/status/um5y5hd2a1pd6xtk/branch/master?svg=true)](https://ci.appveyor.com/project/DanielOliver/flexer/branch/master) | [![NuGet](https://img.shields.io/nuget/v/FLexer.svg)](https://www.nuget.org/packages/FLexer) |


## Getting Started

```
git clone https://github.com/DanielOliver/FLexer.git
cd src/FLexer.Example
dotnet run
```

### Prerequisites

* [.NET Core 2.0 SDK](https://docs.microsoft.com/en-us/dotnet/core/)

## Running the tests

```
git clone https://github.com/DanielOliver/FLexer.git
cd src/FLexer.Tests
dotnet test
```

## Example Code
```fsharp
module FLexer.Example.BasicSQL

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
    SubClassifierBuilder continuation {
        let! status = Classifier.map TokenType.ColumnName IDENTIFIER status
        let (ColumnName columnName) = status.Classification
        return SQLQueryColumn.Column(columnName), status
    }

let AcceptColumnNameWithTableName status continuation =
    SubClassifierBuilder continuation {
        let! status = Classifier.map TokenType.TableName IDENTIFIER status
        let (TableName tableName) = status.Classification
        let! status = Classifier.discard PERIOD status
        let! status = Classifier.map TokenType.ColumnName IDENTIFIER status
        let (ColumnName columnName) = status.Classification
        return SQLQueryColumn.ColumnWithTableName(columnName, tableName), status
    }

let AcceptAllColumnTypes status continuation =
    SubClassifierBuilder continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.discard COMMA status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! (value, status) = ClassifierBuilder.PickOne(status, [ AcceptColumnNameWithTableName; AcceptColumnName ])
        return value, status
    }


let AcceptSQLQuery status =
    RootClassifierBuilder() {
        // Add to token list, but don't return TokenType
        let! status = Classifier.name TokenType.Select SELECT status
        let! status = Classifier.discard WHITESPACE status

        // Add to token list, and return list of TokenTypes. Uses above parsing expression
        let! (column1, status) = ClassifierBuilder.PickOne(status, [ AcceptColumnName; AcceptColumnNameWithTableName ])
        let! (moreColumns, status) = ClassifierBuilder.ZeroOrMore(status, AcceptAllColumnTypes)
        let allColumns = column1 :: (List.rev moreColumns)

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
        |> (FLexer.Example.Utility.PrintBuilderResults (printfn "%A") stringToTest)
    )


//   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***
// -------------------------------------------------------------------------------------
// Accepted "SELECT  LastName, FirstName, ID  , BirthDay  FROM Contacts"
// 
// {Columns =
//   [Column "LastName"; Column "FirstName"; Column "ID"; Column "BirthDay"];
//  Table = "Contacts";}
// 
// --  Consumed Tokens  ----------------------------------------------------------------
//  StartChar  |     EndChar  |                  Text  |                  Classification
// -------------------------------------------------------------------------------------
//          0  |           5  |                SELECT  |  Select
//          8  |          15  |              LastName  |  ColumnName "LastName"
//         18  |          26  |             FirstName  |  ColumnName "FirstName"
//         29  |          30  |                    ID  |  ColumnName "ID"
//         35  |          42  |              BirthDay  |  ColumnName "BirthDay"
//         45  |          48  |                  FROM  |  From
//         50  |          57  |              Contacts  |  TableName "Contacts"
// 
// 
//   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***
// -------------------------------------------------------------------------------------
// Rejected "SELECT Column1, Column2,,,NoColumn FROM Contacts"
// 
// LookaheadFailure
// 
// --  Consumed Tokens  ----------------------------------------------------------------
//  StartChar  |     EndChar  |                  Text  |                  Classification
// -------------------------------------------------------------------------------------
//          0  |           5  |                SELECT  |  Select
// 
// 
//   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***
// -------------------------------------------------------------------------------------
// Accepted "SELECT  Contacts.LastName, FirstName, Contacts.ID  , BirthDay  FROM Contacts"
// 
// {Columns =
//   [ColumnWithTableName ("LastName","Contacts"); Column "FirstName";
//    ColumnWithTableName ("ID","Contacts"); Column "BirthDay"];
//  Table = "Contacts";}
// 
// --  Consumed Tokens  ----------------------------------------------------------------
//  StartChar  |     EndChar  |                  Text  |                  Classification
// -------------------------------------------------------------------------------------
//          0  |           5  |                SELECT  |  Select
//          8  |          15  |              Contacts  |  TableName "Contacts"
//         17  |          24  |              LastName  |  ColumnName "LastName"
//         27  |          35  |             FirstName  |  ColumnName "FirstName"
//         38  |          45  |              Contacts  |  TableName "Contacts"
//         47  |          48  |                    ID  |  ColumnName "ID"
//         53  |          60  |              BirthDay  |  ColumnName "BirthDay"
//         63  |          66  |                  FROM  |  From
//         68  |          75  |              Contacts  |  TableName "Contacts"
// 
// 
//   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***
// -------------------------------------------------------------------------------------
// Accepted "SELECT  LastName  FROM Contacts"
// 
// {Columns = [Column "LastName"];
//  Table = "Contacts";}
// 
// --  Consumed Tokens  ----------------------------------------------------------------
//  StartChar  |     EndChar  |                  Text  |                  Classification
// -------------------------------------------------------------------------------------
//          0  |           5  |                SELECT  |  Select
//          8  |          15  |              LastName  |  ColumnName "LastName"
//         18  |          21  |                  FROM  |  From
//         23  |          30  |              Contacts  |  TableName "Contacts"
// 
// 
//   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***
// -------------------------------------------------------------------------------------
// Accepted "SELECT  Contacts.LastName  FROM Contacts"
// 
// {Columns = [ColumnWithTableName ("LastName","Contacts")];
//  Table = "Contacts";}
// 
// --  Consumed Tokens  ----------------------------------------------------------------
//  StartChar  |     EndChar  |                  Text  |                  Classification
// -------------------------------------------------------------------------------------
//          0  |           5  |                SELECT  |  Select
//          8  |          15  |              Contacts  |  TableName "Contacts"
//         17  |          24  |              LastName  |  ColumnName "LastName"
//         27  |          30  |                  FROM  |  From
//         32  |          39  |              Contacts  |  TableName "Contacts"
// 
// 
//   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***
// -------------------------------------------------------------------------------------
// Accepted "SELECT  LastName , Contacts.FirstName FROM Contacts"
// 
// {Columns = [Column "LastName"; ColumnWithTableName ("FirstName","Contacts")];
//  Table = "Contacts";}
// 
// --  Consumed Tokens  ----------------------------------------------------------------
//  StartChar  |     EndChar  |                  Text  |                  Classification
// -------------------------------------------------------------------------------------
//          0  |           5  |                SELECT  |  Select
//          8  |          15  |              LastName  |  ColumnName "LastName"
//         19  |          26  |              Contacts  |  TableName "Contacts"
//         28  |          36  |             FirstName  |  ColumnName "FirstName"
//         38  |          41  |                  FROM  |  From
//         43  |          50  |              Contacts  |  TableName "Contacts"
```

## Authors

* **Daniel Oliver** - *Initial work* - [DanielOliver](https://github.com/DanielOliver)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

