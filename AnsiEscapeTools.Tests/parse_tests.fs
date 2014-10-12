module parse_tests

open Xunit
open FsUnit.Xunit


open AnsiEscapeTools.Parser

[<Fact>]
let ``parsing empty strings gives empty result back`` () =
    let parser = new AnsiEscapeParser()

    (parser.Parse("")).IsEmpty |> should equal true



