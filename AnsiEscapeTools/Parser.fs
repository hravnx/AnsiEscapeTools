namespace AnsiEscapeTools.Parser

open System


type AnsiEscapeChunk =
    | Empty
    | Text of string

type AnsiEscapeParserResult =
    | Error of (string * int * int)
    | Success of AnsiEscapeChunk seq

type AnsiEscapeParser() =
    member this.Parse (s:string) =
        if(String.IsNullOrEmpty(s)) then
            Success [Empty]
        else
            Success [Text s]


