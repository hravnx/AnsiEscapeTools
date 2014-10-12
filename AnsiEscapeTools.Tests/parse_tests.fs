module parse_tests

open Xunit
open FsUnit.Xunit

open AnsiEscapeTools.Parser


[<AutoOpen>]
module private Helpers =
    open System.Diagnostics

    let seqEq a b = 
        (Seq.length a = Seq.length b) && ((a, b) ||> Seq.forall2 (=))

    let matchSuccess testFun =
        NHamcrest.Func<obj, bool>(fun o ->
            match o :?> AnsiEscapeParserResult with 
            | Success chunks -> testFun chunks
            | _ -> false
            )

    let matchSuccessWith testValue = matchSuccess (seqEq testValue)

    let matcherOf name test =
        new NHamcrest.CustomMatcher<obj>(name, test)

    let emptyResult = matcherOf "empty" (matchSuccessWith [Empty])
    
    let textResult txt = matcherOf "text" (matchSuccessWith [Text txt])

    let parseResult expected = matcherOf "parse" (matchSuccessWith expected)

    let parser = new AnsiEscapeParser()

[<Fact>]
let ``parsing an empty string gives an empty result back`` () =
    parser.Parse("") |> should be emptyResult


[<Fact>]
let ``parsing strings without ansi escapes results in a single Text result`` () =
    parser.Parse("My text without any escapes") |> should be (textResult "My text without any escapes")
    parser.Parse(" \t ") |> should be (textResult " \t ")

[<Fact>]
let ``parsing strings with one or more escape codes works`` () =
    parser.Parse("abc\x1b[2Adef") |> should be (parseResult [Text "abc"; CursorUp 2; Text "def"])
    parser.Parse("abc\x1b[2A") |> should be (parseResult [Text "abc"; CursorUp 2])
    parser.Parse("\x1b[12Adef") |> should be (parseResult [CursorUp 12; Text "def"])
    parser.Parse("abc\x1b[12Adef\x1b[4A") |> should be (parseResult [Text "abc"; CursorUp 12; Text "def"; CursorUp 4])
    parser.Parse("abc\x1b[12A\x1b[4A") |> should be (parseResult [Text "abc"; CursorUp 12; CursorUp 4])

[<Fact>]
let ``parsing CursorUp works`` () =
    parser.Parse("\x1b[A") |> should be (parseResult [CursorUp 1])
    parser.Parse("\x1b[11A") |> should be (parseResult [CursorUp 11])

[<Fact>]
let ``parsing CursorDown works`` () =
    parser.Parse("\x1b[B") |> should be (parseResult [CursorDown 1])
    parser.Parse("\x1b[13B") |> should be (parseResult [CursorDown 13])

[<Fact>]
let ``parsing CursorForward works`` () =
    parser.Parse("\x1b[C") |> should be (parseResult [CursorForward 1])
    parser.Parse("\x1b[3C") |> should be (parseResult [CursorForward 3])

[<Fact>]
let ``parsing CursorRight works`` () =
    parser.Parse("\x1b[D") |> should be (parseResult [CursorBack 1])
    parser.Parse("\x1b[5D") |> should be (parseResult [CursorBack 5])

[<Fact>]
let ``parsing CursorNextLine works`` () =
    parser.Parse("\x1b[E") |> should be (parseResult [CursorNextLine 1])
    parser.Parse("\x1b[7E") |> should be (parseResult [CursorNextLine 7])

[<Fact>]
let ``parsing CursorPrevLine works`` () =
    parser.Parse("\x1b[1F") |> should be (parseResult [CursorPrevLine 1])
    parser.Parse("\x1b[6F") |> should be (parseResult [CursorPrevLine 6])

[<Fact>]
let ``parsing CursorHorizontalAbs works`` () =
    parser.Parse("\x1b[1G") |> should be (parseResult [CursorHorizontalAbs 1])
    parser.Parse("\x1b[2G") |> should be (parseResult [CursorHorizontalAbs 2])

[<Fact>]
let ``parsing CursorPosition works`` () =
    parser.Parse("\x1b[H") |> should be (parseResult [CursorPosition(1, 1)])
    parser.Parse("\x1b[;3H") |> should be (parseResult [CursorPosition(1, 3)])
    parser.Parse("\x1b[2;H") |> should be (parseResult [CursorPosition(2, 1)])
    parser.Parse("\x1b[2H") |> should be (parseResult [CursorPosition(2, 1)])
    parser.Parse("\x1b[2;3H") |> should be (parseResult [CursorPosition(2, 3)])


