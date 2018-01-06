open FLexer.Core

let SELECT = Tokenizer.Consumers.TakeWord "SELECT" true


[<EntryPoint>]
let main argv =
    let stringToAccept = ClassifierStatus<string>.OfString "SELECT"

    stringToAccept
    |> Classifier.name "SELECT" SELECT
    |> printfn "%A"

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
