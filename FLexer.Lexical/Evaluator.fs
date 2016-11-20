module FLexer.Lexical.Evaluator

type [<RequireQualifiedAccess>] Automata =
  | Root of Automata list
  | WildcardCharacter
  | Character of char
  | Range of char * char
  | MultiRange of (char * char) list
  | Or of Automata * Automata
  | Not of Automata
  | Rule of string
  | Word of string
  | OneOrMore of Automata
  | ZeroOrMore of Automata
  | OneOrMoreGreedy of Automata
  | ZeroOrMoreGreedy of Automata
  | Option of Automata


  
type [<RequireQualifiedAccess>] Evaluation =
  | Success of Accepted: int * Remaining: char list
  | Failure of Error: string

type Acceptor = (char list) -> Evaluation


let inline choose (accept1: Acceptor) (accept2: Acceptor): Acceptor =
  (fun pattern -> 
    let a1 = accept1 pattern 
    match a1 with
    | Evaluation.Success(_) -> a1
    | Evaluation.Failure(_) ->
      let a2 = accept2 pattern
      match a2 with
      | Evaluation.Success(_) -> a2
      | Evaluation.Failure(_) -> a1)

let inline private betweenRange first last item = first <= item && item <= last

let rec AcceptAutomata(automata: Automata list) (pattern: char list) =
  match automata, pattern with
  | auto0 :: autoList, pat0 :: patList ->
    match auto0 with
    | Automata.Character(x) ->
      if x = pat0 then Evaluation.Success(1, patList)
      else Evaluation.Failure(sprintf "Expected '%c', received '%c'" x pat0)
    | Automata.Range(first, last) ->
      if betweenRange first last pat0 then Evaluation.Success(1, patList)
      else Evaluation.Failure(sprintf "Expected '%c'-'%c', received '%c'" first last pat0)
    | Automata.MultiRange(ranges) ->
      if ranges |> Seq.exists(fun (first, last) -> betweenRange first last pat0) then Evaluation.Success(1, patList)
      else 
        let errorRange = "[" + System.String.Join(",", ranges |> Seq.map(fun (first, last) -> first.ToString() + "-" + last.ToString())) + "]"
        Evaluation.Failure(sprintf "Expected '%s', received '%c'" errorRange pat0)
    | Automata.WildcardCharacter -> Evaluation.Success(1, patList)
    | Automata.Root(a) -> AcceptAutomata a pattern
    | Automata.Or(first,second) -> 
      let accept1 = AcceptAutomata [ first ] pattern
      match accept1 with
      | Evaluation.Success(_, _) -> accept1
      | Evaluation.Failure(_) ->
        let accept2 = AcceptAutomata [ second ] pattern
        match accept2 with
        | 
    | Automata.Not(_) -> failwith "Not implemented yet"
    | Automata.Rule(_) -> failwith "Not implemented yet"
    | Automata.Word(_) -> failwith "Not implemented yet"
    | Automata.OneOrMore(_) -> failwith "Not implemented yet"
    | Automata.ZeroOrMore(_) -> failwith "Not implemented yet"
    | Automata.OneOrMoreGreedy(_) -> failwith "Not implemented yet"
    | Automata.ZeroOrMoreGreedy(_) -> failwith "Not implemented yet"
    | Automata.Option(_) -> failwith "Not implemented yet"



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

