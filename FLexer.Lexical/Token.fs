namespace FLexer.Lexical

type Token<'m,'t> =
  { Text: string
    Offset: int
    TokenType: 't
    RuleID: int
    Mode: 'm
  }
