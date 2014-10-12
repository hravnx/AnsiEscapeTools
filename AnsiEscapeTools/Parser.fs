namespace AnsiEscapeTools.Parser

open System

type AnsiEscapeSegment =
    | Empty
    | Text of string
    | CursorUp of int
    | CursorDown of int
    | CursorForward of int
    | CursorBack of int

type AnsiEscapeParserResult =
    | Error of (string * int * int)
    | Success of AnsiEscapeSegment seq


[<AutoOpen>]
module private Helpers =
    open System.Text.RegularExpressions

    [<Literal>]
    let escMatcherPattern = "\x1b\\[(\\d*(?:\\;\\d+)?)?([a-zA-Z])"
    [<Literal>]
    let escMatcherOptions = RegexOptions.Compiled ||| RegexOptions.CultureInvariant

    let escapeMatcher = new Regex(escMatcherPattern, escMatcherOptions)

    let escapeCode (letter:string) (args:string) =
        let paramParts = args.Split(';') |> Array.map int
        match letter.[0] with
        | 'A' -> CursorUp paramParts.[0]
        | 'B' -> CursorDown paramParts.[0]
        | 'C' -> CursorForward paramParts.[0]
        | 'D' -> CursorBack paramParts.[0]
        | _ -> failwithf "Unsupported option %s" letter

    let rec parse (s:string) start (ms:MatchCollection) idx = seq {
        if idx >= ms.Count then
            if start < s.Length then yield Text (s.Substring(start))
        else let m = ms.[idx] 
             if start < m.Index then yield Text (s.Substring(start, m.Index-start))
             yield escapeCode m.Groups.[2].Value m.Groups.[1].Value
             yield! parse s (m.Index + m.Length) ms (idx+1)
        }




type AnsiEscapeParser() =

    member this.Parse (s:string) =
        if String.IsNullOrEmpty(s) then Success [Empty]
        else let matches =  escapeMatcher.Matches(s)
             Success (parse s 0 matches 0)
             


