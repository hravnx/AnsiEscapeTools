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





