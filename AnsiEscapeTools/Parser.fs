namespace AnsiEscapeTools.Parser

open System

type AnsiEscapeSegment =
    | Empty
    | Text of string
    | CursorUp of int
    | CursorDown of int
    | CursorForward of int
    | CursorBack of int
    | CursorNextLine of int
    | CursorPrevLine of int
    | CursorHorizontalAbs of int
    | CursorPosition of int * int
    | EraseDisplay of int

type AnsiEscapeParserResult =
    | Error of string * int * int
    | Success of AnsiEscapeSegment list


[<AutoOpen>]
module private Helpers =
    open System.Text.RegularExpressions

    [<Literal>]
    let escMatcherPattern = "\x1b\\[(\\d*(?:;\\d*)?)?([a-zA-Z])"
    [<Literal>]
    let escMatcherOptions = RegexOptions.Compiled ||| RegexOptions.CultureInvariant

    let escapeMatcher = new Regex(escMatcherPattern, escMatcherOptions)

    let argSplitter expected defVal (args:string) =
        let defMap (s:string) =
            if String.IsNullOrWhiteSpace(s) then defVal
            else int (s.Trim())

        let parts = args.Split(';')
        let vals = if parts.Length = 0 then Array.create expected 1
                   else parts |> Array.map defMap
        if vals.Length >= expected then vals
        else Array.concat [|vals; Array.create (expected - vals.Length) 1|]

    let oneArg defVal (args:string) = (argSplitter 1 defVal args).[0]
    let twoArgs defVal (args:string) = 
        let parts = (argSplitter 2 defVal args)
        (parts.[0], parts.[1])
        

    let escapeCode letter arglist =
        match letter with
        | "A" -> CursorUp (arglist |> oneArg 1)
        | "B" -> CursorDown (arglist |> oneArg 1)
        | "C" -> CursorForward (arglist |> oneArg 1)
        | "D" -> CursorBack (arglist |> oneArg 1)
        | "E" -> CursorNextLine (arglist |> oneArg 1)
        | "F" -> CursorPrevLine (arglist |> oneArg 1)
        | "G" -> CursorHorizontalAbs (arglist |> oneArg 1)
        | "H" -> CursorPosition (arglist |> twoArgs 1)
        | "J" -> EraseDisplay (arglist |> oneArg 0)
        | _ -> failwithf "Unsupported option %s" letter

    let rec parse (s:string) start (ms:MatchCollection) (idx:int) = seq {
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
        else let matches = escapeMatcher.Matches(s)
             Success (parse s 0 matches 0 |> Seq.toList)
             


