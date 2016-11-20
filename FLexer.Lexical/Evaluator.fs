module FLexer.Lexical.Evaluator

type [<RequireQualifiedAccess>] Automaton =
  | Root of Automaton list
  | WildcardCharacter
  | Character of char
  | Range of char * char
  | MultiRange of (char * char) list
  | Or of Automaton * Automaton
  | Not of Automaton
  | Rule of string
  | Word of string
  | OneOrMore of Automaton
  | ZeroOrMore of Automaton
  | OneOrMoreGreedy of Automaton
  | ZeroOrMoreGreedy of Automaton
  | Option of Automaton
 
let TranslateAutomaton(pattern: string) = 
  [ Automaton.Range('a', 'z')
    Automaton.Not (Automaton.Range('0', '9'))
    Automaton.ZeroOrMore (Automaton.Range('0', '9'))
  ]

type [<RequireQualifiedAccess>] Evaluation =
  | Success of string
  | Partial of string
  | Failure

type Engine<'t, 'm when 'm: comparison>(rules: Rule<'t, 'm> list) =
  let _rules: Map<'m, Map<string, Rule<'t, 'm>>> = 
    rules
    |> Seq.groupBy(fun rule -> rule.Mode)
    |> Seq.map(fun (mode, rules) -> 
      mode,
      rules 
      |> Seq.map(fun rule -> rule.Name, rule)
      |> Map.ofSeq)
    |> Map.ofSeq

  member this.Evaluate() = 0

