namespace FLexer.Lexical

open System

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

type private AutomataState<'m> = 
  { Mode : 'm list
    Remaining : char list
    Taken : int
    Offset : int }

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
  static member From mode mapper ruleAction name automata order id = 
    { Mode = mode
      Mapper = mapper
      RuleAction = ruleAction
      Name = name
      Automata = automata
      Order = order
      ID = id }

[<RequireQualifiedAccess>]
type EngineFailure = 
  | EOF
  | UnknownMode
  | NoMatchingRule
  | AutomataMismatch

[<RequireQualifiedAccess>]
type private EngineMatch<'m, 't> = 
  | Success of Token<'m, 't> * AutomataState<'m>
  | Failure of EngineFailure

[<RequireQualifiedAccess>]
type private AutomataMatch<'m> = 
  | Success of AutomataState<'m>
  | Failure of EngineFailure

[<RequireQualifiedAccess>]
type EngineResult<'m, 't> = 
  | Success of Token<'m, 't> list
  | Failure of Token<'m, 't> list * EngineFailure

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
         |> Seq.toList)
    |> Map.ofSeq
  
  let charListToString x = 
    x
    |> List.toArray
    |> System.String
  
  let evaluateAutomata (automata : Automata) (state : AutomataState<'m>) = 
    match automata with
    | Automata.Character(x) -> 
      match state.Remaining with
      | next :: remaining when next = x -> 
        AutomataMatch.Success({ state with Remaining = remaining
                                           Taken = state.Taken + 1 })
      | _ -> AutomataMatch.Failure(EngineFailure.AutomataMismatch)
    | Automata.Word(x) -> 
      if x.Length > state.Remaining.Length then AutomataMatch.Failure(EngineFailure.AutomataMismatch)
      else 
        let wordMatch, remaining = state.Remaining |> List.splitAt (x.Length)
        let matchedText = charListToString wordMatch
        if matchedText.Equals(x, StringComparison.InvariantCultureIgnoreCase) then 
          AutomataMatch.Success({ state with Remaining = remaining
                                             Taken = state.Taken + matchedText.Length })
        else AutomataMatch.Failure(EngineFailure.AutomataMismatch)
    | _ -> AutomataMatch.Failure(EngineFailure.AutomataMismatch)
  
  let evaluateRule (rule : Rule<'m, 't>) (state : AutomataState<'m>) = 
    let automataResult = evaluateAutomata rule.Automata state
    match automataResult with
    | AutomataMatch.Failure(x) -> EngineMatch.Failure x
    | AutomataMatch.Success(newState) -> 
      let tokenText : string = 
        state.Remaining
        |> List.take (newState.Taken)
        |> charListToString
      
      let tokenResult = 
        { Token.Offset = newState.Offset
          Token.Text = tokenText
          Token.RuleID = rule.ID
          Token.TokenType = rule.Mapper tokenText
          Mode = state.Mode.Head }
      
      let nextMode = 
        match rule.RuleAction with
        | Some(RuleAction.PushMode(x)) -> x :: state.Mode
        | Some(RuleAction.PopMode) -> state.Mode.Tail
        | _ -> state.Mode
      
      let newState = 
        { newState with Offset = newState.Offset + newState.Taken
                        Mode = nextMode
                        Taken = 0 }
      
      EngineMatch.Success(tokenResult, newState)
  
  let nextToken (state : AutomataState<'m>) = 
    match orderLookup.TryFind state.Mode.Head with
    | Some(rules) -> 
      let rec evaluateNextRule (remainingRules : Rule<'m, 't> list) = 
        match remainingRules with
        | [] -> EngineMatch.Failure EngineFailure.NoMatchingRule
        | head :: tail -> 
          let nextResult = evaluateRule head state
          match nextResult with
          | EngineMatch.Success(token, newState) -> nextResult
          | EngineMatch.Failure(_) -> evaluateNextRule tail
      evaluateNextRule rules
    | None -> EngineMatch.Failure EngineFailure.UnknownMode
  
  member this.ParseString (text : string) initialmode = 
    let rec readToken (state : AutomataState<'m>) (tokens : Token<'m, 't> list) = 
      if state.Remaining.IsEmpty then EngineResult.Success(tokens |> List.rev)
      else 
        match nextToken (state) with
        | EngineMatch.Success(token, newState) -> readToken newState (token :: tokens)
        | EngineMatch.Failure(fail) -> EngineResult.Failure(tokens |> List.rev, fail)
    readToken { Offset = 0
                Remaining = text |> Seq.toList
                Mode = [ initialmode ]
                Taken = 0 } []
  
  member this.ID0 = nameLookup
  member this.ID1 = idLookup
  member this.ID2 = orderLookup
