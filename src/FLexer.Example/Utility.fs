module FLexer.Example.Utility

open FLexer.Core

let PrintTokens (consumed: Tokenizer.Token<_> list) =
    printfn "--  Consumed Tokens  ----------------------------------------------------------------"
    printfn "%10s  |  %10s  |  %20s  |  %30s" "StartChar" "EndChar" "Text" "Classification"
    printfn "-------------------------------------------------------------------------------------"
    consumed |> List.rev |> List.iter (fun t -> printfn "%10i  |  %10i  |  %20s  |  %30A" t.StartCharacter t.EndCharacter t.Text t.Classification)

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
        PrintTokens err.LastStatus.Consumed

