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


let example = "
let Four = 5.0;
let Method(param: int): bool {
  if (param == 0) then { true; } else { false; }
}
"

[<EntryPoint>]
let main argv = 
//  let tokens = Tokenize rules example
//
//  for i in tokens do
//    printfn "%A" i.TokenType

  System.Console.Read() |> ignore
  0 // return an integer exit code
