module FLexer.Example.Program

[<EntryPoint>]
let main argv =

    FLexer.Example.Core.BasicSQL.Example()
    FLexer.Example.Core.JSON.Example()

    0 // return an integer exit code
