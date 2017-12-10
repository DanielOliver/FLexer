namespace FLexer.Core

/// Represents the universe of errors
[<RequireQualifiedAccess>]
type SimpleTokenErrors =
    | NoPatternsInTokenList

/// Represents a much simplified regex.
[<RequireQualifiedAccess>]
type SimpleToken =
    /// A single letter to match
    | Letter of char
    ///An inclusive range of letters
    | Range of Start: char * End: char
    /// Anything but this letter to match
    | ExclusiveLetter of char
    /// An exclusive range of letters
    | ExclusiveRange of Start: char * End: char


type MultiSimpleToken = SimpleToken list

/// Represents what the token parser is looking for.
[<RequireQualifiedAccess>]
type SimpleTokenPattern =
    | Inclusive
    | Exclusive
    | InclusiveAndExclusive

    static member Of simpleToken =
        match simpleToken with
        | SimpleToken.Letter _ -> Inclusive
        | SimpleToken.Range _ -> Inclusive
        | SimpleToken.ExclusiveLetter _ -> Exclusive
        | SimpleToken.ExclusiveRange _ -> Exclusive

    static member OfMultiple (simpleTokens: MultiSimpleToken) =
        let distinctItems = simpleTokens |> Seq.map SimpleTokenPattern.Of |> Set.ofSeq

        let hasInclusive = distinctItems |> Set.contains Inclusive
        let hasExclusive = distinctItems |> Set.contains Exclusive
        if hasInclusive && hasExclusive then
            InclusiveAndExclusive
        else if hasInclusive then
            Inclusive
        else if hasExclusive then
            Exclusive
        else
            // This is only reached if empty list.
            InclusiveAndExclusive
