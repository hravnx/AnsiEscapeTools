module parse_tests

open Xunit
open FsUnit.Xunit


open AnsiEscapeTools.Parser


[<AutoOpen>]
module private Helpers =
    let seqEq a b = (a, b) ||> Seq.forall2 (=)

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


let parser = new AnsiEscapeParser()

[<Fact>]
let ``parsing an empty string gives an empty result back`` () =
    parser.Parse("") |> should be emptyResult


[<Fact>]
let ``parsing strings without ansi escapes results in a single Text result`` () =
    parser.Parse("My text without any escapes") |> should be (textResult "My text without any escapes")
    parser.Parse(" \t ") |> should be (textResult " \t ")




