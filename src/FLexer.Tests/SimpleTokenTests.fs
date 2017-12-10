namespace FLexer.Tests.Tests

open NUnit.Framework
open FLexer.Core

[<SimpleTokenTests>]
type SimpleTokenTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.``Assure that Simplifying ranges is permissive.`` () =
        let items =
            [   SimpleToken.ExclusiveLetter 'a'         
                SimpleToken.Letter 'c'
            ]
        let inclusives = SimpleParser.SimplifyTokenList items
                
        Assert.AreEqual(2, inclusives.Length)
        let first = inclusives.[0]
        let second = inclusives.[1]
        Assert.IsTrue(first.Start = System.Char.MinValue && first.End = '`', "First range is min to `")
        Assert.IsTrue(second.End = System.Char.MaxValue && second.Start = 'b', "Second range is b to max")

