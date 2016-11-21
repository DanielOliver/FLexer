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

[<RequireQualifiedAccess>]
type EngineFailure = 
  | EOF
  | UnknownMode
  | NoMatchingRule

[<RequireQualifiedAccess>]
type EngineMatch<'m, 't> = 
  | Success of Token<'t> * AutomataState<'m>
  | Failure of EngineFailure

[<RequireQualifiedAccess>]
type private AutomataMatch<'m> = 
  | Success of AutomataState<'m>
  | Failure of EngineFailure

[<RequireQualifiedAccess>]
type EngineResult<'m, 't> = 
  | Success of Token<'t> list
  | Failure of Token<'t> list * EngineFailure

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
  
  let evaluateAutomata (automata : Automata) (state : AutomataState<'m>) = AutomataMatch.Success state
  
  let evaluateRule (rule : Rule<'m, 't>) (state : AutomataState<'m>) = 
    let automataResult = evaluateAutomata rule.Automata state
    match automataResult with
    | AutomataMatch.Failure(x) -> EngineMatch.Failure x
    | AutomataMatch.Success(newState) -> 
      let tokenText : string = 
        state.Remaining
        |> List.take (newState.Taken)
        |> List.toArray
        |> System.String
      
      let tokenResult = 
        { Token.Offset = newState.Offset
          Token.Text = tokenText
          Token.TokenType = rule.Mapper tokenText }
      
      let newState = { newState with Offset = newState.Offset + newState.Taken }
      EngineMatch.Success(tokenResult, newState)
  
  let nextToken (state : AutomataState<'m>) = 
    match orderLookup.TryFind state.Mode with
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
  
  member this.ParseString (text : string) (initialmode : 'm) = 
    let rec readToken (state : AutomataState<'m>) (tokens : Token<'t> list) = 
      if state.Remaining.IsEmpty then EngineResult.Success(tokens |> List.rev)
      else 
        match nextToken (state) with
        | EngineMatch.Success(token, newState) -> readToken newState (token :: tokens)
        | EngineMatch.Failure(fail) -> EngineResult.Failure(tokens |> List.rev, fail)
    readToken { Offset = 0
                Remaining = text |> Seq.toList
                Mode = initialmode
                Taken = 0 } []
  
  member this.ID0 = nameLookup
  member this.ID1 = idLookup
  member this.ID2 = orderLookup
