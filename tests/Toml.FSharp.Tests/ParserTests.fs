#if INTERACTIVE
#r @"../../packages/NUnit/lib/net45/nunit.framework.dll"
#r @"../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r @"../../packages/FParsec/lib/net40-client/FParsec.dll"
#r @"../../packages/FsCheck/lib/net45/FsCheck.dll"
#r @"../../src/toml-fs/bin/release/toml-fs.dll"
#load   "Prelude.fs"
        "Generators.fs"
#else
module TomlFs.Tests.ParserTests
#endif


open NUnit.Framework
open FsCheck
open FParsec 
open TomlFSharp.Prelude
open TomlFSharp.Parsers
open TomlFs.Tests.Prelude
open TomlFs.Tests.Generators

let inline throwConfig maxTest startSize endSize = 
    { Config.QuickThrowOnFailure with MaxTest = maxTest; StartSize = startSize; EndSize = endSize}

let longCheck   x = Check.One (throwConfig 10000 50 700, x)
let midCheck    x = Check.One (throwConfig 3000  20 300, x)
let shortCheck  x = Check.One (throwConfig 500   5  100, x)


(*|---------------------|*)
(*| String Parser Tests |*)
(*|---------------------|*)


let parserTest bound parser  =
    fun (str:string) -> 
        (str.Length > bound) ==>
            match parseString parser str with
            | ParserResult.Success(_,_,_) -> true
            | ParserResult.Failure(_,_,_) -> false 

let stringParser psr = parserTest 6 psr

let [<Test>] ``parses all basic strings`` () =
    longCheck <| Prop.forAll basic_string_arb (stringParser basic_string)


let [<Test>] ``parses all multi strings`` () =
    longCheck <| Prop.forAll multi_string_arb (stringParser multi_string)
    

let [<Test>] ``parses all literal strings`` () =
    longCheck <| Prop.forAll literal_string_arb (stringParser literal_string)


let [<Test>] ``parses all multi literal strings`` () =
    longCheck <| Prop.forAll multi_lit_string_arb (stringParser multi_literal_string)


let [<Test>] ``unified string parser reads all toml string types`` () =
    midCheck <| Prop.forAll toml_string_arb (stringParser toml_string)


(*|---------------------------|*)
(*| Simple Value Parser Tests |*)
(*|---------------------------|*)


let valueParser psr = parserTest 3 psr

let [<Test>] ``parses all ints`` () =
    Check.QuickThrowOnFailure <| Prop.forAll toml_int_arb (valueParser toml_int)


let [<Test>] ``parses all floats`` () =
    Check.QuickThrowOnFailure <| Prop.forAll toml_float_arb (valueParser toml_float)


let [<Test>] ``parses bools`` () =
    Check.Quick <| Prop.forAll toml_bool_arb (valueParser toml_bool )


let [<Test>] ``parses all DateTimes`` () =
    Check.QuickThrowOnFailure <| Prop.forAll toml_datetime_arb (valueParser toml_datetime)


let [<Test>] ``parses all Arrays`` () =
    Check.QuickThrowOnFailure <| Prop.forAll toml_array_arb (valueParser toml_array)


(*|--------------------|*)
(*| Table Parser Tests |*)
(*|--------------------|*)


let [<Test>] ``parses keys for table elements `` () =
    shortCheck <| Prop.forAll toml_key_arb (valueParser toml_key)


let [<Test>] ``parses bare table keys`` () =
    shortCheck <| Prop.forAll toml_bareTableKey_arb (valueParser pBareKey)


let [<Test>] ``parses quote table keys`` () =
    shortCheck <| Prop.forAll toml_quoteTableKey_arb (valueParser pQuoteKey)


let [<Test>] ``parses toml keys`` () =
    shortCheck <| Prop.forAll toml_key_arb (valueParser toml_key)


let [<Test>] ``parses toml items (key value pairs)`` () =
    Check.QuickThrowOnFailure <| Prop.forAll toml_item_arb (valueParser toml_item)


(*|-------------------------|*)
(*| Array Size 0 or 1 Tests |*)
(*|-------------------------|*)
let runArray0Or1 test =
    let reply = run section_splitter test
    match reply with 
    | Failure (str,_,_) -> failwith str
    | Success (res,_,_) -> ()   
 

let [<Test>] ``parses arrays of size 0`` () =
    let test = """[package]

    b = [ ]
    """
    runArray0Or1 test

let [<Test>] ``parses arrays of size 1`` () =
    let test = """[package]

    b = [ 10 ]
    """
    runArray0Or1 test


#if INTERACTIVE
// Test Switches
let stringTests      = false
let simpleValueTests = false

if stringTests then 
    ``parses all basic strings`` ()
    ``parses all multi strings`` ()
    ``parses all literal strings`` ()
    ``parses all multi literal strings`` () 
    ``unified string parser reads all toml string types`` ()

if simpleValueTests then
    ``parses all ints`` ()
    ``parses all floats`` ()
    ``parses bools`` () 
    ``parses all DateTimes``()
    ``parses all Arrays`` ()


``parses keys for table elements `` ()
``parses bare table keys`` ()
``parses quote table keys`` () 
``parses toml keys`` () 
``parses toml items (key value pairs)`` ()
#endif

