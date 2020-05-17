namespace Tessa.LexTests

open System
open Xunit
open Tessa.Lex

module LexTests = 
    module Lex = Lex

    let fromResult = function
        | Error e -> failwith (e.ToString())
        | Ok o -> o

    [<Fact>]
    let ``Lex an entire statement`` () =
        let statement = "[] i1 i2 = ((a + b) |- 1/2 (c + d) @ 67/89);"
        let lexed = Lex.lex statement |> fromResult |> List.map fst
        let expected = [Lex.PrimitiveProc(Lex.ArrayBuilder); Lex.Identifier("i1"); Lex.Identifier("i2"); Lex.PrimitiveProc(Lex.Assign);
            Lex.BeginNestedExpression; Lex.BeginNestedExpression; 
            Lex.Identifier("a"); Lex.PrimitiveProc(Lex.LinkPoints); Lex.Identifier("b");
            Lex.EndNestedExpression; 
            Lex.PrimitiveProc(Lex.Perpendicular); Lex.Fraction(1, 2); 
            Lex.BeginNestedExpression; Lex.Identifier("c"); Lex.PrimitiveProc(Lex.LinkPoints); Lex.Identifier("d"); Lex.EndNestedExpression; 
            Lex.PrimitiveProc(Lex.At); Lex.Fraction(67, 89); Lex.EndNestedExpression; 
            Lex.EndStackOps;]
        Assert.Equal<Lex.Token list>(lexed, expected);

    [<Fact>]
    let ``Lex an entire statement 2`` () =
        let statement = "(! (a.b))"
        let lexed = Lex.lex statement |> fromResult |> List.map fst
        let expected = [Lex.BeginNestedExpression; Lex.PrimitiveProc(Lex.Draw); Lex.BeginNestedExpression;
            Lex.Identifier("a"); Lex.PrimitiveProc(Lex.RecordAccess); Lex.Identifier("b"); Lex.EndNestedExpression; Lex.EndNestedExpression;]
        Assert.Equal<Lex.Token list>(lexed, expected);

    [<Fact>]
    let ``Lex identifier with dash`` () =
        let s = "c4-clockwise"
        let lexed = Lex.lex s |> fromResult |> List.map fst
        let expected = [Lex.Identifier("c4-clockwise")] 
        Assert.Equal<Lex.Token list>(expected, lexed)

    [<Fact>]
    let ``Lex String`` () =
        let s = "\"0047b3\""
        let lexed = Lex.lex s |> fromResult |> List.map fst
        let expected = [Lex.String "0047b3"]
        Assert.Equal<Lex.Token list>(expected, lexed)

