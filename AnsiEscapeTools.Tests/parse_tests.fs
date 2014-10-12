module parse_tests

open Xunit
open FsUnit.Xunit

open AnsiEscapeTools.Parser


[<AutoOpen>]
module private Helpers =

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

    let resultIn expected = matcherOf "parse" (matchSuccessWith expected)

    let parser = new AnsiEscapeParser()

    let parsing s = parser.Parse(s)

    let parsing' s = (fun () -> parser.Parse(s) |> ignore)


[<Fact>]
let ``unknown command throws an exception`` () =
    parsing' "\x1b[12Y" |> should throw typeof<System.Exception>


[<Fact>]
let ``an empty string gives an empty result back`` () =
    parsing "" |> should resultIn [Empty]


[<Fact>]
let ``strings without ansi escapes results in a single Text result`` () =
    parsing "My text without any escapes" |> should resultIn [Text "My text without any escapes"]
    parsing " \t " |> should resultIn [Text " \t "]


[<Fact>]
let ``strings with one or more escape codes works`` () =
    parsing "abc\x1b[2Adef" |> should resultIn [Text "abc"; CursorUp 2; Text "def"]
    parsing "abc\x1b[2A" |> should resultIn [Text "abc"; CursorUp 2]
    parsing "\x1b[12Adef" |> should resultIn [CursorUp 12; Text "def"]
    parsing "abc\x1b[12Adef\x1b[4A" |> should resultIn [Text "abc"; CursorUp 12; Text "def"; CursorUp 4]
    parsing "abc\x1b[12A\x1b[4A" |> should resultIn [Text "abc"; CursorUp 12; CursorUp 4]


[<Fact>]
let ``CursorUp works`` () =
    parsing "\x1b[A" |> should resultIn [CursorUp 1]
    parsing "\x1b[11A" |> should resultIn [CursorUp 11]

[<Fact>]
let ``CursorDown works`` () =
    parsing "\x1b[B" |> should resultIn [CursorDown 1]
    parsing "\x1b[13B" |> should resultIn [CursorDown 13]

[<Fact>]
let ``CursorForward works`` () =
    parsing "\x1b[C" |> should resultIn [CursorForward 1]
    parsing "\x1b[3C" |> should resultIn [CursorForward 3]

[<Fact>]
let ``CursorRight works`` () =
    parsing "\x1b[D" |> should resultIn [CursorBack 1]
    parsing "\x1b[5D" |> should resultIn [CursorBack 5]

[<Fact>]
let ``CursorNextLine works`` () =
    parsing "\x1b[E" |> should resultIn [CursorNextLine 1]
    parsing "\x1b[7E" |> should resultIn [CursorNextLine 7]

[<Fact>]
let ``CursorPrevLine works`` () =
    parsing "\x1b[F" |> should resultIn [CursorPrevLine 1]
    parsing "\x1b[6F" |> should resultIn [CursorPrevLine 6]

[<Fact>]
let ``CursorHorizontalAbs works`` () =
    parsing "\x1b[G" |> should resultIn [CursorHorizontalAbs 1]
    parsing "\x1b[2G" |> should resultIn [CursorHorizontalAbs 2]

[<Fact>]
let ``CursorPosition works`` () =
    parsing "\x1b[H" |> should resultIn [CursorPosition(1, 1)]
    parsing "\x1b[;3H" |> should resultIn [CursorPosition(1, 3)]
    parsing "\x1b[2;H" |> should resultIn [CursorPosition(2, 1)]
    parsing "\x1b[2H" |> should resultIn [CursorPosition(2, 1)]
    parsing "\x1b[2;3H" |> should resultIn [CursorPosition(2, 3)]

[<Fact>]
let ``EraseDisplay works`` () =
    parsing "\x1b[J" |> should resultIn [EraseDisplay 0]
    parsing "\x1b[1J" |> should resultIn [EraseDisplay 1]
    parsing "\x1b[2J" |> should resultIn [EraseDisplay 2]

[<Fact>]
let ``EraseInLine`` () =
    parsing "\x1b[K" |> should resultIn [EraseInLine 0]
    parsing "\x1b[1K" |> should resultIn [EraseInLine 1]
    parsing "\x1b[2K" |> should resultIn [EraseInLine 2]


