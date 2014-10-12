namespace AnsiEscapeTools.Parser


type AnsiEscapeChunk =
    | Empty
    | Text of string

type AnsiEscapeParserResult =
    | Error of (string * int * int)
    | Success of AnsiEscapeChunk seq

type AnsiEscapeParser() =
    member this.Parse (s:string) =
        Success [Empty]


