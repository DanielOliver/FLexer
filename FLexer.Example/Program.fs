open FLexer.Lexical


type [<RequireQualifiedAccess>] Keyword =
  | Let
  | False
  | True
  | If
  | Else
  | Then

type [<RequireQualifiedAccess>] Operator =
  | EqualTo
  | GreaterThan
  | LessThan
  | GreaterThanOrEqualTo
  | LessThanOrEqualTo
  | Multiply
  | Divide
  | Assign

type [<RequireQualifiedAccess>] Punctuation =
  | OpenCurlyBracket
  | CloseCurlyBracket
  | OpenBracket
  | CloseBracket
  | OpenParentheses
  | CloseParentheses
  | SemiColon

type [<RequireQualifiedAccess>] TokenType =
  | Whitespace
  | Keyword of Keyword
  | Integer of int
  | Decimal of decimal
  | Identifier of string
  | StringLiteral of string
  | NewLine
  | Operator of Operator
  | CustomOperator of string
  | Punctuation of Punctuation


type [<RequireQualifiedAccess>] LexerMode =
  | Normal
  | Arrow
  
let rules = 
  [| Rule.From LexerMode.Normal TokenType.Identifier None "Space Normal" (Automata.Character(' ')) 0 0
     Rule.From LexerMode.Arrow TokenType.Identifier None "Space Arrow" (Automata.Character(' ')) 0 1
     Rule.From LexerMode.Normal TokenType.Identifier (Some(RuleAction.PushMode(LexerMode.Arrow))) "<" (Automata.Character('<')) 0 2
     Rule.From LexerMode.Arrow TokenType.Identifier (Some(RuleAction.PopMode)) ">" (Automata.Character('>')) 0 3
  |]

[<EntryPoint>]
let main argv = 
  let engine = Engine rules
  
  let example = "  <  > >  "
  let engineResult = engine.ParseString example LexerMode.Normal

  let tokens =
    match engineResult with
    | EngineResult.Failure(tokens, fail) -> 
      printfn "Failure: %A" fail
      tokens
    | EngineResult.Success(tokens) -> 
      printfn "Success"
      tokens

  for i in tokens do
    printfn "%A" i

  System.Console.Read() |> ignore
  0 // return an integer exit code
