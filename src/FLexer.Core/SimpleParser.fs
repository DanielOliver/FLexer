module FLexer.Core.SimpleParser


type InclusiveRange = 
    {   Start: char
        End: char        
    }

    /// Checks if two inclusive ranges overlap
    static member IsOverlap (range1: InclusiveRange) (range2: InclusiveRange) =
        (range1.Start >= range2.Start && range1.Start <= range2.End)
        || (range1.End >= range2.Start && range1.End <= range2.End)

    /// Combines two inclusive ranges into one. Hopefully they overlap
    static member Combine (range1: InclusiveRange) (range2: InclusiveRange) =
        {   Start = char <| System.Math.Min( int range1.Start, int range2.Start )
            End =  char <| System.Math.Min( int range2.End, int range2.End )
        }

    /// Maps any simple token into a list of Inclusive
    static member ToInclusive simpleToken =
        match simpleToken with
        | SimpleToken.Letter letter -> List.singleton { Start = letter; End = letter }
        | SimpleToken.Range (first, last) -> List.singleton { Start = first; End = last }
        | SimpleToken.ExclusiveLetter letter -> [ { Start = System.Char.MinValue; End = char((int letter) - 1) }; { Start = char((int letter) + 1); End = System.Char.MaxValue } ]
        | SimpleToken.ExclusiveRange (first, last) -> [ { Start = System.Char.MinValue; End = char((int first) - 1) }; { Start = char((int last) + 1); End = System.Char.MaxValue } ]

    /// Condenses multiple inclusive ranges into one.
    static member CombineMultiple (rangeList: InclusiveRange list) =
        let rec combine ranges condensed =
            match ranges with
            | range1 :: tail ->
                let (overlaps, noOverlaps) = condensed |> List.partition (InclusiveRange.IsOverlap range1)
                let condensedOverlaps = if overlaps.IsEmpty then range1 else (overlaps |> List.fold InclusiveRange.Combine range1)
                let newCondensed = condensedOverlaps :: noOverlaps
                combine tail newCondensed
            | [] -> condensed
        combine rangeList []

/// Simplifies a list of tokens to be inclusive
let SimplifyTokenList (tokens: MultiSimpleToken) =
    tokens 
    |> List.collect InclusiveRange.ToInclusive 
    |> InclusiveRange.CombineMultiple
    |> List.sortBy(fun t -> t.Start)
