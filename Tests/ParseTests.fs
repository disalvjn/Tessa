namespace Tessa.ParseTests

open Tessa.Parse
open Tessa.Lex
open Xunit

module ParseTests = 
    module Lex = Lex
    module P = Parse

    let failAndPrint a = failwith (sprintf "%A" a)

    let fromResult = function
        | Error e -> failAndPrint e
        | Ok o -> o

    let lexAndParse s = 
        let lexed = fromResult <| Lex.lex s 
        let parsed = Parse.parseList <| List.map fst lexed
        fromResult parsed

    [<Fact>]
    let ``Test Nested Expressions`` () = 
        let result = fst <| lexAndParse "1 :plus 2 :plus (3 :plus 4) :plus 5"
        let expected = 
            [P.Expression (P.Number 1.0);
            P.ReduceAndPushOp None;
            P.Expression (P.Identifier "plus");
            P.Expression (P.Number 2.0);
            P.ReduceAndPushOp None;
            P.Expression (P.Identifier "plus");
            P.NewStack
                [P.Expression (P.Number 3.0);
                P.ReduceAndPushOp None;
                P.Expression (P.Identifier "plus");
                 P.Expression (P.Number 4.0);];
            P.ReduceAndPushOp None;
            P.Expression (P.Identifier "plus");
            P.Expression (P.Number 5.0);]
        Assert.Equal<P.StackCommand list>(expected, result)

    [<Fact>]
    let ``Test Quoted Simple Expressions`` () =     
        let result = fst <| lexAndParse "'i eq 1"
        let expected = 
            ["i" |> P.Identifier |> P.Expression |> P.Quote |> P.Expression;
            P.Expression (P.Identifier "eq");
            P.Expression (P.Number 1.0);]
        Assert.Equal<P.StackCommand list>(expected, result)

    [<Fact>]
    let ``Test Quoted Nested Expressions`` () =
        let result = fst <| lexAndParse "f '(g a) b"
        let expected = 
            [P.Expression(P.Identifier "f");
            P.Expression (P.Quote (P.NewStack [
                P.Expression (P.Identifier "g");
                P.Expression (P.Identifier "a");
            ]));
            P.Expression (P.Identifier "b");]
        Assert.Equal<P.StackCommand list>(expected, result)