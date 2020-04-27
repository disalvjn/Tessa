namespace Tessa.EvalTests

open Xunit
open Tessa.Eval 
open Tessa.Lex 
open Tessa.Parse 

module EvalTests = 
    module Lex = Lex 
    module Parse = Parse
    module E = Eval

    let failAndPrint a = failwith (sprintf "%A" a)

    let fromResult = function
        | Error e -> failAndPrint e
        | Ok o -> o

    let fromNumber = function
        | E.Number n -> n
        | x -> failAndPrint x

    let fromSomeNumber = function 
        | Some(E.Number n) -> n
        | x -> failAndPrint x

    let lexAndParse s = 
        let lexed = fromResult <| Lex.lex s 
        List.map fst lexed |>  Parse.parseList |> fromResult |> fst

    let eval s = E.eval (lexAndParse s)

    // [<Fact>]
    // let ``Simple Addition Nested Incomplete`` () = 
    //     // failAndPrint <| E.flattenParseStackCommands (lexAndParse "1 :plus ( 2")
    //     failAndPrint <| eval "1 :plus (( 2 :plus 3) :plus 4"
    //     ()

    [<Fact>]
    let ``Addition With Continuation Returning`` () = 
        let result = eval "1 :plus (2);" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(3.0, asNum)

    [<Fact>]
    let ``Simple Addition`` () = 
        let result = eval "1 :plus 2 :plus 3;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(6.0, asNum)

    [<Fact>]
    let ``Simple Addition Nested`` () = 
        let result = eval "1 :plus ((2 :plus 3) :plus 4);" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(10.0, asNum)

    [<Fact>]
    let ``Trivial Statements`` () = 
        let result = eval "1; 2;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(2.0, asNum)

    [<Fact>]
    let ``Simple Addition Multiple Statements`` () = 
        let result = eval "1 :plus 2; 2 :plus 3;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(5.0, asNum)

    [<Fact>]
    let ``Simple Assignment Returns rvalue`` () = 
        let result = eval "'i = 1;" |> fromResult
        let asNum = fromSomeNumber <| result.currentContext.ret
        Assert.Equal(1.0, asNum)

    [<Fact>]
    let ``Simple Assignment Then Lookup`` () = 
        let result = eval "'i = 1; i;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(1.0, asNum)

    [<Fact>]
    let ``Test Record Building 1``() =
        let input = 
            """
            'x = 1; 'y = 2;
            'r = ({} 'x 'y);
             """
        let result = eval input |> fromResult
        let (Some(E.Record(recordMap))) = result.currentContext.ret 
        let expected = Map.empty |> Map.add "x" (E.Number 1.0) |> Map.add "y" (E.Number 2.0)
        Assert.Equal<Map<string, E.Exp>>(expected, recordMap)
        
    [<Fact>]
    let ``Test Record Building 2``() =
        let input = 
            """
            'x = 1;
            'r = ({} 'x 'y 2);
             """
        let result = eval input |> fromResult
        let (Some(E.Record(recordMap))) = result.currentContext.ret 
        let expected = Map.empty |> Map.add "x" (E.Number 1.0) |> Map.add "y" (E.Number 2.0)
        Assert.Equal<Map<string, E.Exp>>(expected, recordMap)

    [<Fact>]
    let ``Test Record Access``() =
        let input = 
            """
            'r = ({} 'x 1 'y 2);
            r . 'x :plus (r . 'y);
             """
        let result = eval input |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret 
        Assert.Equal(3.0, asNum)
        


