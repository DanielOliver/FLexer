open FLexer.Core
open FLexer.Core.Tokenizer

let SELECT = Consumers.TakeWord "SELECT" true

let FROM = Consumers.TakeWord "FROM" true

let WHITESPACE = Consumers.TakeRegex "\s+" 

let IDENTIFIER = Consumers.TakeRegex "[A-Za-z][A-Za-z0-9]*"


[<EntryPoint>]
let main argv =
    let stringToAccept = ClassifierStatus<string>.OfString "SELECT  Column1 FROM"

    stringToAccept
    |> Classifier.name "SELECT" SELECT
    |> Result.bind (Classifier.discard WHITESPACE)
    |> Result.bind (Classifier.map id IDENTIFIER)
    |> Result.bind (Classifier.discard WHITESPACE)
    |> Result.bind (Classifier.name "FROM" FROM)
    |> function
        | Ok t -> t.Consumed |> List.rev |> List.iter (printfn "%A")
        | Error err -> printfn "%A" err

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
