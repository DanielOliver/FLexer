namespace FLexer.Lexical

open System

type AutomataState<'m> = 
  { Mode : 'm list
    Remaining : string
    Taken : int
    Offset : int }

[<RequireQualifiedAccess>]
type RuleType = 
  | Fragment
  | Complete

[<RequireQualifiedAccess>]
type RuleAction<'m> = 
    ///Push the given mode after processing this match.
  | PushMode of 'm
    ///Change this match to use the given mode.
  | PushModeBefore of 'm
    ///Pop the given mode after processing this match.
  | PopMode
    ///Change this match to use the next mode.
  | PopModeBefore

type Rule<'m, 't> = 
  { Mode : 'm
    Mapper : string -> 't
    RuleAction : RuleAction<'m> option
    Name : string
    Regex : System.Text.RegularExpressions.Regex
    Order : int
    ID : int }
  static member Create mode mapper ruleAction name regex order id = 
    { Mode = mode
      Mapper = mapper
      RuleAction = ruleAction
      Name = name
      Regex = regex
      Order = order
      ID = id } 
      
  static member From mode mapper ruleAction name regex = 
    { Mode = mode
      Mapper = mapper
      RuleAction = ruleAction
      Name = name
      Regex = regex
      Order = 0
      ID = 0 } 

  static member Initialize (rules: Rule<_,_> seq) =
    rules 
    |> Seq.mapi(fun index item -> { item with Order = index; ID = index })
    |> Seq.toArray

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
type AutomataMatch<'m> = 
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
    |> Seq.toArray
    |> System.String
  
  let evaluateRegex (regex : System.Text.RegularExpressions.Regex) (state : AutomataState<'m>) = 
    match regex.Match(state.Remaining) with
    | x when x.Success && x.Index = 0 -> 
        AutomataMatch.Success ({ state with Remaining = state.Remaining.Substring(x.Length);
                                            Taken = x.Length
                               })
    | _ -> AutomataMatch.Failure EngineFailure.EOF

  
  let evaluateRule (rule : Rule<'m, 't>) (state : AutomataState<'m>) = 
    let automataResult = evaluateRegex rule.Regex state
    match automataResult with
    | AutomataMatch.Failure(x) -> EngineMatch.Failure x
    | AutomataMatch.Success(newState) -> 
      let tokenText : string = state.Remaining.Substring(0, newState.Taken)
      
      let nextModeStack, tokenResultMode = 
        let currentHead = state.Mode.Head
        match rule.RuleAction with
        | Some(RuleAction.PushMode(x)) -> 
            x :: state.Mode, currentHead
        | Some(RuleAction.PushModeBefore(x)) -> 
            x :: state.Mode, x
        | Some(RuleAction.PopMode) -> 
            state.Mode.Tail, currentHead
        | Some(RuleAction.PopModeBefore) ->
            let tail = state.Mode.Tail
            tail, tail.Head
        | None -> state.Mode, currentHead

      let tokenResult = 
        { Token.Offset = newState.Offset
          Token.Text = tokenText
          Token.RuleID = rule.ID
          Token.TokenType = rule.Mapper tokenText
          Mode = tokenResultMode }
      
      
      let newState = 
        { newState with Offset = newState.Offset + newState.Taken
                        Mode = nextModeStack
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
      if state.Remaining.Length = 0 then EngineResult.Success(tokens |> List.rev)
      else 
        match nextToken (state) with
        | EngineMatch.Success(token, newState) -> readToken newState (token :: tokens)
        | EngineMatch.Failure(fail) -> EngineResult.Failure(tokens |> List.rev, fail)
    readToken { Offset = 0
                Remaining = text
                Mode = [ initialmode ]
                Taken = 0 } []
  
  member this.ID0 = nameLookup
  member this.ID1 = idLookup
  member this.ID2 = orderLookup
