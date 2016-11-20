open FLexer.Lexical.Tokenizer


type [<RequireQualifiedAccess>] Keyword =
  | Let
  | False
  | True

type [<RequireQualifiedAccess>] Operator =
  | EqualTo
  | GreaterThan
  | LessThan
  | GreaterThanOrEqualTo
  | LessThanOrEqualTo
  | Multiply
  | Divide

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

let rules =
  [ Rule.Literal("let", fun _ -> TokenType.Keyword(Keyword.Let))
    Rule.Literal("true", fun _ -> TokenType.Keyword(Keyword.True))
    Rule.Literal("false", fun _ -> TokenType.Keyword(Keyword.False))

    Rule.Regex(@"[0-9]+[.][0-9]+", System.Decimal.Parse >> TokenType.Decimal)
    Rule.Regex(@"[0-9]+", System.Int32.Parse >> TokenType.Integer)
    Rule.Regex(@"[A-Za-z][0-9A-Za-z]*", TokenType.Identifier)
    Rule.Regex(@"[ ]+", fun _ -> TokenType.Whitespace)
    Rule.Regex(@"(\r\n|\n|\r)", fun _ -> TokenType.NewLine)
    
    Rule.Literal("[", fun _ -> TokenType.Punctuation Punctuation.OpenBracket)
    Rule.Literal("]", fun _ -> TokenType.Punctuation Punctuation.CloseBracket)
    Rule.Literal("{", fun _ -> TokenType.Punctuation Punctuation.OpenCurlyBracket)
    Rule.Literal("}", fun _ -> TokenType.Punctuation Punctuation.CloseCurlyBracket)
    Rule.Literal("(", fun _ -> TokenType.Punctuation Punctuation.OpenParentheses)
    Rule.Literal(")", fun _ -> TokenType.Punctuation Punctuation.CloseParentheses)
    Rule.Literal(";", fun _ -> TokenType.Punctuation Punctuation.SemiColon)

    Rule.Literal("=", fun _ -> TokenType.Operator Operator.EqualTo)
    Rule.Literal(">", fun _ -> TokenType.Operator Operator.GreaterThan)
    Rule.Literal("<", fun _ -> TokenType.Operator Operator.LessThan)
    Rule.Literal(">=", fun _ -> TokenType.Operator Operator.GreaterThanOrEqualTo)
    Rule.Literal("<=", fun _ -> TokenType.Operator Operator.LessThanOrEqualTo)
    Rule.Literal("/", fun _ -> TokenType.Operator Operator.Divide)
    Rule.Literal("*", fun _ -> TokenType.Operator Operator.Multiply)
    
    Rule.Regex(@"[!@#$%^&*<>?:~\-=_\/]+", TokenType.CustomOperator)
    Rule.Regex("[\"]([^\"]|[\\\\][\"])*[\"]", TokenType.StringLiteral)
  ]



let example = "
let Four = 5.0;
"

[<EntryPoint>]
let main argv = 
  let tokens = Tokenize rules example

  for i in tokens do
    printfn "%A" i

  System.Console.Read() |> ignore
  0 // return an integer exit code
