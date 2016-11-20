namespace FLexer.Lexical

type [<RequireQualifiedAccess>] RuleType =
  | Token
  | Fragment


type [<RequireQualifiedAccess>] RuleAction<'m> =
  | PushMode of 'm
  | PopMode


type Rule<'t, 'm> =
  { Order: int
    Name: string
    Mode: 'm
    Mapper: (string -> 't)
    Pattern: string
    RuleType: RuleType
    RuleAction: RuleAction<'m> option
  }
  



