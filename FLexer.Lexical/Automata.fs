namespace FLexer.Lexical

open System

type private AutomataState<'m> = 
  { Mode : 'm list
    Remaining : string
    Taken : int
    Offset : int }
    
type EngineSetup =
  { Rules: Rule list
  }
  member this.AddRule name regex =
    let newRule = { Rule.ID = this.Rules.Length; Regex = regex; Name = name }
    { this with
        Rules = newRule  :: this.Rules
    }, newRule
and Rule = 
  { Name : string
    Regex : System.Text.RegularExpressions.Regex
    ID : int }

[<RequireQualifiedAccess>]
type EngineFailure = 
  | EOF
  | UnknownMode
  | NoMatchingRule

[<RequireQualifiedAccess>]
type private EngineMatch = 
  | Success of Token * AutomataState<'m>
  | Failure of EngineFailure
  

[<RequireQualifiedAccess>]
type EngineResult = 
  | Success of Token list
  | Failure of Token list * EngineFailure



type Engine<'m, 't when 'm : comparison>(setup: EngineSetup) = 
  
  let nameLookup = 
    setup.Rules
    |> Seq.map (fun rule -> (rule.Name), rule)
    |> Map.ofSeq
  
  let idLookup = 
    setup.Rules
    |> Seq.map (fun rule -> rule.ID, rule)
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

  
  member this.ParseString (text : string) initialmode = 
    EngineResult.Failure(List.empty, EngineResult.Failure)

  
  member this.NameLookup = nameLookup
  member this.IDLookup = idLookup
