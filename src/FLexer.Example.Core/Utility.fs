module FLexer.Example.Core.Utility

open FLexer.Core

let PrintTokens (consumed: Tokenizer.Token<_> list) =
    printfn "--  Consumed Tokens  ----------------------------------------------------------------"
    printfn "%10s  |  %10s  |  %20s  |  %30s" "StartChar" "EndChar" "Text" "Classification"
    printfn "-------------------------------------------------------------------------------------"
    consumed |> List.rev |> List.iter (fun t -> printfn "%10i  |  %10i  |  %20s  |  %30A" t.StartCharacter t.EndCharacter t.Text t.Classification)

let PrintConsumedWords (consumed: string list) =
    printfn "--  Consumed Text  ------------------------------------------------------------------"
    printfn "%30s %10s" "Text" "Length"
    printfn "-------------------------------------------------------------------------------------"
    let whitespacePrint text = if System.String.IsNullOrWhiteSpace text then "(Whitespace)" else text
    consumed |> List.rev |> List.iter (fun t -> printfn "%30s  |  %10i" (whitespacePrint t) t.Length)

let Spacer() =
    printfn ""
    printfn ""
    printfn "  ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***   ***  "
    printfn "-------------------------------------------------------------------------------------"

let PrintBuilderResults formatValue stringToAccept (results: ClassifierBuilderResult<_,_>) =
    Spacer()
    match results with
    | Ok(value, status) -> 
        printfn "Accepted \"%s\"" stringToAccept
        printfn ""
        formatValue value
        printfn ""
        PrintTokens status.Consumed
    | Error err ->
        printfn "Rejected \"%s\"" stringToAccept
        printfn ""
        match err.TokenizerError with
        | None -> ()
        | Some x ->
            printfn "%A" x
            printfn ""        
        PrintConsumedWords err.LastStatus.ConsumedWords

