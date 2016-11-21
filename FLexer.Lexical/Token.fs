namespace FLexer.Lexical

type Token<'t> =
  { Text: string
    Offset: int
    TokenType: 't
    RuleID: int
  }
