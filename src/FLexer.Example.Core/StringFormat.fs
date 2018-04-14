module FLexer.Example.Core.StringFormat


open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Core.Classifier

type Tree =
    | TextFragment of Text: string
    | VariableFragment of VariableName: string
    | Root of Tree list


let leftCurlyBracket = Consumers.TakeChar '{'
let rightCurlyBracket = Consumers.TakeChar '}'
let atSymbol = Consumers.TakeChar '@'
let plainTextRegex = Consumers.TakeRegex "[^{]+"
let identifierLabel =  Consumers.TakeRegex "[A-Za-z]+"


let ReadVariable status continuation =
    Classifiers.sub continuation {
        let! status = discard leftCurlyBracket status
        let! status = discard atSymbol status
        let! status = name "variable" identifierLabel status
        let variableName = status.ConsumedText
        let! status = discard rightCurlyBracket status

        return (Tree.VariableFragment variableName), status
    }


let ReadPlainText status continuation =
    Classifiers.sub continuation {
        let! status = name "plainText" plainTextRegex status
        let textFragment = status.ConsumedText

        return (Tree.TextFragment textFragment), status
    }

let ReadTreeFragment status continuation =
    Classifiers.sub continuation {
        let! (fragment, status) = ClassifierFunction.PickOne([ ReadPlainText; ReadVariable ]) status
        return fragment, status
    }

let ReadTree status =
    Classifiers.root() {
        let! (treeFragments, status) = ClassifierFunction.ZeroOrMore(ReadTreeFragment) status
        return Tree.Root(List.rev treeFragments), status
    }


let ExampleTester = ClassifierStatus<string>.OfString >> ReadTree

/// True if the string should be accepted, false if should be rejected.
let ExampleStrings =
    [   true, "{@variable}"
        false, "{variable}"
        false, "{}"
        true, "Hello, {@firstName}, good evening!"
        true, "{@productName} are great!"
    ]

let Example() =
    ExampleStrings
    |> List.iter(fun (_, stringToTest) ->
        stringToTest
        |> ExampleTester
        |> (FLexer.Example.Core.Utility.PrintBuilderResults (printfn "%A") stringToTest)
    )

