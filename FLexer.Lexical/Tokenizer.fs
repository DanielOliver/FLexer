module FLexer.Lexical.Tokenizer

type Token<'t> =
  { Text: string
    Offset: int
    TokenType: 't
  }
