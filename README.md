# FLexer

FLexer is a F# Lexer and Parser dedicated to ease of use and expressiveness. Creating Domain Specific Languages within F# and .NET is an excellent use-case.

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
open FLexer.Core
open FLexer.Core.Tokenizer


/// ######  Lexer words & regex  ######
let SELECT = Consumers.TakeWord "SELECT" true
let FROM = Consumers.TakeWord "FROM" true
let WHITESPACE = Consumers.TakeRegex "\s+" 
let COMMA = Consumers.TakeChar ','
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
        do! Discard COMMA
        do! Discard OPTIONAL_WHITESPACE
        let! (TokenType.ColumnName columnName) = Classifier.map TokenType.ColumnName IDENTIFIER
        return columnName
    }
        
let AcceptSQLQuery status =
    ClassifierBuilder status {
        // Add to token list, but don't return TokenType
        do! Accept(Classifier.name TokenType.Select SELECT)
        do! Discard WHITESPACE
        
        // Add to token list, and return list of TokenTypes. Uses above parsing expression
        let! (TokenType.ColumnName columnName1) = Classifier.map TokenType.ColumnName IDENTIFIER
        let! moreColumns = ZeroOrMore AcceptColumnName
        let allColumns = columnName1 :: moreColumns

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


[<EntryPoint>]
let main argv =
    let stringToAccept = "SELECT  LastName, FirstName, ID  , BirthDay  FROM Contacts"

    stringToAccept
    |> ClassifierStatus<string>.OfString 
    |> AcceptSQLQuery
    |> (function
        | Ok(value, status) -> 
            printfn "Accepted \"%s\"" stringToAccept
            printfn ""
            printfn "Query - %A" value
            printfn ""
            printfn "Tokens -------------"
            printfn "%10s  |  %10s  |  %20s  |  %30s" "StartChar" "EndChar" "Text" "Classification"
            printfn "-----------------------------------------------------------------------------"
            status.Consumed |> List.rev |> List.iter (fun t -> printfn "%10i  |  %10i  |  %20s  |  %30A" t.StartCharacter t.EndCharacter t.Text t.Classification)
        | Error err -> printfn "%A" err)
    
    0 // return an integer exit code


// Accepted "SELECT  LastName, FirstName, ID  , BirthDay  FROM Contacts"
// 
// Query - {Columns = ["LastName"; "BirthDay"; "ID"; "FirstName"];
//  Table = "Contacts";}
// 
// Tokens -------------
//  StartChar  |     EndChar  |                  Text  |                  Classification
// -----------------------------------------------------------------------------
//          0  |           5  |                SELECT  |  Select
//          8  |          15  |              LastName  |  ColumnName "LastName"
//         18  |          26  |             FirstName  |  ColumnName "FirstName"
//         29  |          30  |                    ID  |  ColumnName "ID"
//         35  |          42  |              BirthDay  |  ColumnName "BirthDay"
//         45  |          48  |                  FROM  |  From
//         50  |          57  |              Contacts  |  TableName "Contacts"
```

## Authors

* **Daniel Oliver** - *Initial work* - [DanielOliver](https://github.com/DanielOliver)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

