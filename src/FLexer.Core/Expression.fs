namespace FLexer.Core

type Expression = 
    | Char of char
    | CharRange of char * char
    | Literal of string    
    | Regex of string

type Token<'t> = 
    {   StartCharacter: int
        EndCharacter: int
        Text: string
        Classification: 't
    }

