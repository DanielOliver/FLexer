namespace FLexer.Lexical

type Token<'t> =
  { Text: string
    Offset: int
    TokenType: 't
  }

//type [<RequireQualifiedAccess>] Rule<'t> =
//  | Regex of Pattern: string * Mapper: (string -> 't)
//  | Literal of Literal: string * Mapper: (string -> 't)
//
//
//type [<RequireQualifiedAccess>] EngineMatch<'t> =
//  | Success of Token<'t>
//  | Failure of string
//
//
//type Engine<'t>(rules: Rule<'t> list ) =
//  let _rules = 
//    rules 
//    |> Seq.mapi(fun index t -> index, t) 
//    |> Map.ofSeq
//  let _regex = 
//    _rules 
//    |> Seq.choose(fun t -> match t.Value with | Rule.Regex (pattern, mapping) -> Some(t.Key, pattern, mapping) | _ -> None)
//    |> Seq.map(fun (index, pattern, mapping) -> index, (new System.Text.RegularExpressions.Regex("\G" + pattern, System.Text.RegularExpressions.RegexOptions.Compiled)))
//    |> Map.ofSeq
//
//  let _occurrences (getFromString: string) (checkedChar: char) (maxChar: int) =
//    let rec loop i count = 
//      if i < getFromString.Length || i < maxChar then
//        if getFromString.[i] = checkedChar then loop (i+1) (count+1)
//        else loop (i+1) count
//      else count
//    loop 0 0
//
////  let _occurrences (x: string, c: char) x |> Seq.filter()
//  let _line (x: string) (startAt: int) =  _occurrences x '\n' startAt
//  let _column (x: string) (startAt: int) =
//    match x.LastIndexOf('\n', startAt) with
//    | -1 -> startAt
//    | y -> startAt - y
//
//  let _getToken (startAt: int) (text: string) =
//    _rules
//    |> Seq.choose(fun t ->
//      let rule = t.Value
//      let position = t.Key
//      let translate (creator: string -> 't) (sample: string) = 
//        try creator sample 
//        with | _ as ex -> failwithf "Failed to translate \"%s\" with \"%s\"" sample ex.Message
//      match rule with 
//      | Rule.Regex(pattern, mapper) ->
//        let ruleRegex = _regex.[position]
//        match ruleRegex.Match(text, startAt) with
//        | null -> None
//        | x when x.Success -> 
//          Some( 
//            { Text = x.Value
//              TokenType = translate mapper x.Value
//              Offset = startAt
//            }, position)
//        | _ -> None
//      | Rule.Literal(literal, mapper) ->
//        if startAt + literal.Length > text.Length then None
//        else
//          let substr = text.Substring(startAt, literal.Length)
//          match substr.Equals(literal, System.StringComparison.InvariantCultureIgnoreCase) with
//          | false -> None
//          | true ->
//            Some(
//              { Text = substr
//                TokenType = translate mapper substr
//                Offset = startAt                                          
//               }, position)
//    ) 
//    |> Seq.sortByDescending(fun (t, position) -> t.Text.Length, System.Int32.MaxValue - position)
//    |> Seq.tryHead
//    |> (function | Some(x, _) -> x | None -> failwithf "Failed to find rule for position %i." startAt)
//
//  member this.TokenStream (text: string) =
//    let rec nextToken startAt =
//      let token = _getToken startAt text
//      let nextOffset = token.Text.Length + token.Offset
//      if nextOffset = text.Length then [ token ]
//      else token :: (nextToken nextOffset)  
//    nextToken 0
//    
//let Tokenize<'t>(rules: Rule<'t> list) (text: string) =
//  let engine = Engine rules
//  engine.TokenStream text
//
//
