namespace FLexer.Lexical

[<RequireQualifiedAccess>]
type Automata = 
  | WildcardCharacter
  | Character of char
  | CharactorEvaluation of (char -> bool)
  | Word of string
  | Fragment of string
  | Root of Automata list
  | Option of Automata
  | Not of Automata
  | Or of Automata * Automata
  | OneOrMore of Automata
  | ZeroOrMore of Automata
  | OneOrMoreGreedy of Automata
  | ZeroOrMoreGreedy of Automata

type AutomataState<'m> = 
  { Mode : 'm
    Remaining : char list
    Taken : int
    Offset : int }

[<RequireQualifiedAccess>]
type AutomataResult<'m> = 
  | Success of Taken : int * State : AutomataState<'m>
  | Failure of Error : string

type Acceptor<'m> = AutomataState<'m> -> AutomataResult<'m>

[<RequireQualifiedAccess>]
type RuleType = 
  | Fragment
  | Complete

[<RequireQualifiedAccess>]
type RuleAction<'m> = 
  | PushMode of 'm
  | PopMode

type Rule<'m, 't> = 
  { Mode : 'm
    Mapper : string -> 't
    RuleAction : RuleAction<'m> option
    Name : string
    Automata : Automata
    Order : int
    ID : int }

type Engine<'m, 't when 'm : comparison>(rules : Rule<'m, 't> array) = 
  
  let nameLookup = 
    rules
    |> Seq.map (fun rule -> (rule.Mode, rule.Name), rule)
    |> Map.ofSeq
  
  let idLookup = 
    rules
    |> Seq.map (fun rule -> rule.ID, rule)
    |> Map.ofSeq
  
  let orderLookup = 
    rules
    |> Seq.groupBy (fun rule -> rule.Mode)
    |> Seq.map (fun (mode, r) -> 
         mode, 
         r
         |> Seq.sortBy (fun t -> t.Order, t.ID)
         |> Seq.toArray)
    |> Map.ofSeq
  
  let evaluateAutomata (automata : Automata) (state : AutomataState<'m>) = 
    state
  
  let evaluateRule (rule : Rule<'m, 't>) (state : AutomataState<'m>) = 
    let newState : AutomataState<'m> = evaluateAutomata rule.Automata state
    
    let tokenText: string = 
      state.Remaining
      |> List.take (newState.Taken)
      |> List.toArray
      |> System.String
    
    let tokenResult = 
      { Token.Offset = newState.Offset
        Token.Text = tokenText
        Token.TokenType = rule.Mapper tokenText }
    
    let newState = { newState with Offset = newState.Offset + newState.Taken }
    (tokenResult, newState)
  
  member this.ID0 = nameLookup
  member this.ID1 = idLookup
  member this.ID2 = orderLookup
