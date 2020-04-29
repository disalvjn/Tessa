namespace Tessa.EvalTests

open Xunit
open Tessa.Eval.Types
open Tessa.Eval 
open Tessa.Lex 
open Tessa.Parse 

module EvalTests = 
    module Lex = Lex 
    module Parse = Parse
    module E = Eval
    module E = EvalTypes

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

    let evalAll s = snd <| E.eval (lexAndParse s)
    let evalPartial s = fst <| E.eval (lexAndParse s)

    // [<Fact>]
    // let ``Simple Addition Nested Incomplete`` () = 
    //     // failAndPrint <| E.flattenParseStackCommands (lexAndParse "1 :plus ( 2")
    //     failAndPrint <| eval "1 :plus (( 2 :plus 3) :plus 4"
    //     ()

    [<Fact>]
    let ``Addition With Continuation Returning`` () = 
        let result = evalAll "1 :plus (2);" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(3.0, asNum)

    [<Fact>]
    let ``Simple Addition`` () = 
        let result = evalAll "1 :plus 2 :plus 3;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(6.0, asNum)

    [<Fact>]
    let ``Simple Addition Nested`` () = 
        let result = evalAll "1 :plus ((2 :plus 3) :plus 4);" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(10.0, asNum)

    [<Fact>]
    let ``Trivial Statements`` () = 
        let result = evalAll "1; 2;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(2.0, asNum)

    [<Fact>]
    let ``Simple Addition Multiple Statements`` () = 
        let result = evalAll "1 :plus 2; 2 :plus 3;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(5.0, asNum)

    [<Fact>]
    let ``Simple Assignment Returns rvalue`` () = 
        let result = evalAll "'i = 1;" |> fromResult
        let asNum = fromSomeNumber <| result.currentContext.ret
        Assert.Equal(1.0, asNum)

    [<Fact>]
    let ``Simple Assignment Then Lookup`` () = 
        let result = evalAll "'i = 1; i;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(1.0, asNum)

    [<Fact>]
    let ``Test Record Building 1``() =
        let input = 
            """
            'x = 1; 'y = 2;
            'r = ({} 'x 'y);
             """
        let result = evalAll input |> fromResult
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
        let result = evalAll input |> fromResult
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
        let result = evalAll input |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret 
        Assert.Equal(3.0, asNum)

    [<Fact>]
    let ``Test Incomplete Current Expression Return`` () =

        Assert.Equal(9.0, fromSomeNumber <| evalPartial "2 :plus 3 :plus ('i = 4);")

        Assert.Equal(5.0, fromSomeNumber <| evalPartial "2 :plus 3 :plus ('i = ")

        Assert.Equal(9.0, fromSomeNumber <| evalPartial "2 :plus 3 :plus ('i = 4")

        Assert.Equal(9.0, fromSomeNumber <| evalPartial "2 :plus 3 :plus ('i = 4) :plus ('x")

        Assert.Equal(4.0, fromSomeNumber <| evalPartial "('i = 4 :plus")

        Assert.Equal(4.0, fromSomeNumber <| evalPartial "('i = 4 :plus 'x")

        Assert.Equal(9.0, fromSomeNumber <| evalPartial "2 :plus 3 :plus ('i = 4 :plus")

        // todo: This is controversial.
        // This could either evaluate to 9 or 5. It depends what we do with 
        // ('i = 4 :plus 'x
        // You could argue that the top frame is not complete, so we should only take into account
        // the continuation, which can reduce. This gives us 5.
        // Or you could argue that while the top frame cannot currently reduce, it was able to previously,
        // and we should be using that reduction and passing it to the continuation, giving us 9.
        // I think 9 makes the most sense because if it's being entered sequentially, it would
        // go 5 -> 9 (at 'i = 4) -> _, and going back to 5 seems like bad form.
        Assert.Equal(9.0, fromSomeNumber <| evalPartial "2 :plus 3 :plus ('i = 4 :plus ('x")

        Assert.Equal(10.0, fromSomeNumber <| evalPartial "2 :plus 3 :plus ('i = 4 :plus ('x = 1")
        
    // Assignment:
    // Mostly care: REcord <- Array and Array <- Array
    [<Fact>]
    let ``Test Array Piecewise Assignment`` () =
        let program = 
            """
            [] 'a 'b 'c = ([] 1 2 3);
            'a 'b 'c :plus;
            """
        let result = evalAll program |> fromResult
        Assert.Equal(6.0, result.currentContext.ret |> fromSomeNumber)


