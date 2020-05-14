namespace Tessa.EvalTests

open Xunit
open Tessa.Eval.Types
open Tessa.Eval 
open Tessa.Lex 
open Tessa.Parse 
open Tessa.Language

module EvalTests = 
    module Lex = Lex 
    module Parse = Parse
    module E = Eval
    module E = EvalTypes
    module L = Language

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

    let eval s = (lexAndParse s |> E.eval).value
    let evalResult s = (lexAndParse s |> E.eval)

    [<Fact>]
    let ``Addition With Continuation Returning`` () = 
        let result = eval "1 :plus (2);" |> fromSomeNumber
        Assert.Equal(3.0, result)

    [<Fact>]
    let ``Simple Addition`` () = 
        let result = eval "1 :plus 2 :plus 3;" |> fromSomeNumber
        Assert.Equal(6.0, result)

    [<Fact>]
    let ``Simple Addition Nested`` () = 
        let result = eval "1 :plus ((2 :plus 3) :plus 4);" |> fromSomeNumber
        Assert.Equal(10.0, result)

    [<Fact>]
    let ``Trivial Statements`` () = 
        let result = eval "1; 2;" |> fromSomeNumber
        Assert.Equal(2.0, result)

    [<Fact>]
    let ``Simple Addition Multiple Statements`` () = 
        let result = eval "1 :plus 2; 2 :plus 3;" |> fromSomeNumber
        Assert.Equal(5.0, result)

    [<Fact>]
    let ``Simple Assignment Returns rvalue`` () = 
        let result = eval "'i = 1;" |> fromSomeNumber
        Assert.Equal(1.0, result)

    [<Fact>]
    let ``Simple Assignment Then Lookup`` () = 
        let result = eval "'i = 1; i;" |> fromSomeNumber
        Assert.Equal(1.0, result)

    [<Fact>]
    let ``Test Record Building 1``() =
        let input = 
            """
            'x = 1; 'y = 2;
            'r = ({} 'x 'y);
             """
        let result = eval input 
        let (Some(E.Record(recordMap))) = result 
        let expected = Map.empty |> Map.add "x" (E.Number 1.0) |> Map.add "y" (E.Number 2.0)
        Assert.Equal<Map<string, E.Exp>>(expected, recordMap)
        
    [<Fact>]
    let ``Test Record Building 2``() =
        let input = 
            """
            'x = 1;
            'r = ({} 'x 'y 2);
             """
        let result = eval input
        let (Some(E.Record(recordMap))) = result
        let expected = Map.empty |> Map.add "x" (E.Number 1.0) |> Map.add "y" (E.Number 2.0)
        Assert.Equal<Map<string, E.Exp>>(expected, recordMap)

    [<Fact>]
    let ``Test Record Access``() =
        let input = 
            """
            'r = ({} 'x 1 'y 2);
            r . 'x :plus (r . 'y);
             """
        let result = eval input |> fromSomeNumber
        Assert.Equal(3.0, result)

    [<Fact>]
    let ``Test Incomplete Current Expression Return`` () =

        Assert.Equal(9.0, fromSomeNumber <| eval "2 :plus 3 :plus ('i = 4);")

        Assert.Equal(5.0, fromSomeNumber <| eval "2 :plus 3 :plus ('i = ")

        Assert.Equal(9.0, fromSomeNumber <| eval "2 :plus 3 :plus ('i = 4")

        Assert.Equal(9.0, fromSomeNumber <| eval "2 :plus 3 :plus ('i = 4) :plus ('x")

        Assert.Equal(4.0, fromSomeNumber <| eval "('i = 4 :plus")

        Assert.Equal(4.0, fromSomeNumber <| eval "('i = 4 :plus 'x")

        Assert.Equal(9.0, fromSomeNumber <| eval "2 :plus 3 :plus ('i = 4 :plus")

        // todo: This is controversial.
        // This could either evaluate to 9 or 5. It depends what we do with 
        // ('i = 4 :plus 'x
        // You could argue that the top frame is not complete, so we should only take into account
        // the continuation, which can reduce. This gives us 5.
        // Or you could argue that while the top frame cannot currently reduce, it was able to previously,
        // and we should be using that reduction and passing it to the continuation, giving us 9.
        // I think 9 makes the most sense because if it's being entered sequentially, it would
        // go 5 -> 9 (at 'i = 4) -> _, and going back to 5 seems like bad form.
        Assert.Equal(9.0, fromSomeNumber <| eval "2 :plus 3 :plus ('i = 4 :plus ('x")

        Assert.Equal(10.0, fromSomeNumber <| eval "2 :plus 3 :plus ('i = 4 :plus ('x = 1")
        
    // Assignment:
    // Mostly care: REcord <- Array and Array <- Array
    [<Fact>]
    let ``Test Array Piecewise Assignment`` () =
        let program = 
            """
            [] 'a 'b 'c = ([] 1 2 3);
            a b c :plus;
            """
        let result = eval program |> fromSomeNumber
        Assert.Equal(6.0, result)

    [<Fact>]
    let ``Test Complete Draw`` () = 
        let program =
            """
            [] 'a 'b 'c 'd = (:square);
            a + b !'k;
            (c + d !'k);
            """
        let er = evalResult program 
        let x = er.runtime.drawMap

        let (expected: E.DrawMap) = Map.add (E.CellName "k") [
                L.Link(L.Absolute(1.0, 0.0), L.Absolute(0.0, 0.0)) |> E.LSegment;
                L.Link(L.Absolute(0.0, 1.0), L.Absolute(1.0, 1.0)) |> E.LSegment;] Map.empty

        Assert.Equal<E.DrawMap>(expected, er.runtime.drawMap)

    [<Fact>]
    let ``Test Incomplete Draw`` () = 
        let program =
            """
            [] 'a 'b 'c 'd = (:square);
            a b + !'k;
            (c + d !'k; 'i = )
            """
        let er = evalResult program 

        let (expected: E.DrawMap) = Map.add (E.CellName "k") [
                L.Link(L.Absolute(1.0, 0.0), L.Absolute(0.0, 0.0)) |> E.LSegment;
                L.Link(L.Absolute(0.0, 1.0), L.Absolute(1.0, 1.0)) |> E.LSegment;] Map.empty

        Assert.Equal<E.DrawMap>(expected, er.runtime.drawMap)


    [<Fact>]
    let ``Test Eval`` () =
        let program = 
            """
            1 :plus 2 is 'x;
            :eval '(x :plus 5);
            """
        Assert.Equal(8.0, fromSomeNumber <| eval program)

    [<Fact>]
    let ``Test Eval Incomplete`` () =
        let program = 
            """
            1 :plus 2 is 'x;
            :eval '(x :plus 5 :plus;
            """
        Assert.Equal(8.0, fromSomeNumber <| eval program)

    [<Fact>]
    let ``Test Trivial Lambda`` () =
        let program = 
            """
            'x 'y -> '(x :plus y) is 'f;
            3 4 :f;
            """
        Assert.Equal(7.0, fromSomeNumber <| eval program)
        
    [<Fact>]
    let ``Test Closure`` () =
        let program = 
            """
            'x -> '('y -> '(x :plus y)) is 'make-adder;
            :make-adder 3 is 'add3;
            :add3 4;
            """
        Assert.Equal(7.0, fromSomeNumber <| eval program)

    [<Fact>]
    let ``Test Simple Dynamic Binding`` () =
        let program = 
            """
            'x -> '(x :plus &add-me) is 'dyn-add;
            '&add-me 5 <> '(:dyn-add 6);
            """

        // failAndPrint <| evalResult program

        Assert.Equal(11.0, fromSomeNumber <| eval program)
        
