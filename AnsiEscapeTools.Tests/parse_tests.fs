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
    

[<Fact>]
let ``parsing an empty string gives an empty result back`` () =
    let parser = new AnsiEscapeParser()
    parser.Parse("") |> should be emptyResult



