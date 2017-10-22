open FLexer.Lexical

type Regex = System.Text.RegularExpressions.Regex
type RegOpt = System.Text.RegularExpressions.RegexOptions

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
  | StringDelimiter of string
  | NewLine
  | Operator of Operator
  | CustomOperator of string
  | Punctuation of Punctuation

  member this.NoFun = (fun (arg: string) -> this)


type [<RequireQualifiedAccess>] LexerMode =
  | Normal
  | Arrow
  | String
  | Delimiters
  
let rules: Rule<LexerMode, TokenType> array = 
  [  Rule.From LexerMode.Normal TokenType.Whitespace.NoFun None "Whitespace" (Regex("[^\S\r\n]+"))
     Rule.From LexerMode.Normal TokenType.NewLine.NoFun None "Newline" (Regex("(\r|\n|\r\n)+"))
     Rule.From LexerMode.Normal (TokenType.Keyword Keyword.Let).NoFun None "Space Normal" (Regex("(?i)Let"))
     Rule.From LexerMode.Arrow TokenType.Identifier None "Space Arrow" (Regex("[ ]+"))

     Rule.From LexerMode.Normal (TokenType.Operator Operator.GreaterThan).NoFun (Some(RuleAction.PushModeBefore(LexerMode.Arrow))) "<" (Regex("[<]"))
     Rule.From LexerMode.Arrow (TokenType.Operator Operator.LessThan).NoFun (Some(RuleAction.PopMode)) ">" (Regex("[>]"))

     
     Rule.From LexerMode.Normal (fun delimiter -> TokenType.StringDelimiter delimiter) (Some(RuleAction.PushMode(LexerMode.String))) "StringLiteralDelimiterIn" (Regex("[\"]{3}"))
     Rule.From LexerMode.String (fun literal -> TokenType.StringLiteral literal) None "StringLiteral" (Regex("(.|\s)+?(?=[\"]{3})"))
     Rule.From LexerMode.String (fun delimiter -> TokenType.StringDelimiter delimiter) (Some(RuleAction.PopModeBefore)) "StringLiteralDelimiterOut" (Regex("[\"]{3}"))
  ] |> Rule<LexerMode, TokenType>.Initialize

[<EntryPoint>]
let main argv = 
  let engine = Engine rules
  
  let example = "let < > <>          \r\n   \"\"\"I could be a string\r\n \"\"\"   "
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
