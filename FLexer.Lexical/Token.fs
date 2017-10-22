namespace FLexer.Lexical

type Token =
  { Text: string
    Offset: int
    TokenType: string
    RuleID: int
  }
