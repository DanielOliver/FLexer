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
  | StringLiteral

let example = "
let Four = 5.0;
let Method(param: int): bool {
  if (param == 0) then { true; } else { false; }
}"


let rules = 
  [| Rule.From LexerMode.Normal TokenType.Identifier None "Space" (Automata.Character(' ')) 0 0
     Rule.From LexerMode.Normal TokenType.Identifier None "C" (Automata.Character('c')) 0 0
  |]

[<EntryPoint>]
let main argv = 
  let engine = Engine rules
  let engineResult = engine.ParseString " c ccc  " LexerMode.Normal

  let tokens =
    match engineResult with
    | EngineResult.Failure(tokens, fail) -> 
      printfn "Failure: %A" fail
      tokens
    | EngineResult.Success(tokens) -> 
      printfn "Success"
      tokens

  for i in tokens do
    printfn "%A" i.TokenType

  System.Console.Read() |> ignore
  0 // return an integer exit code
