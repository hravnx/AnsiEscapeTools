namespace AnsiEscapeTools.Parser



type AnsiEscapeParserResult() =
    member val IsEmpty = true with get, set
    

type AnsiEscapeParser() =
    member this.Parse (s:string) =
        new AnsiEscapeParserResult()


