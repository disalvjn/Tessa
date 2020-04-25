namespace Tessa.LexTests

open System
open Xunit
open Tessa.Lex

module LexTests = 
    module Lex = Lex

    [<Fact>]
    let ``Lex an entire statement`` () =
        let statement = "[] i1 i2 = ((a + b) |- 1/2 (c + d) @ 67/89);"
        let lexed = Lex.lex statement |> List.map fst
        let expected = [Lex.ArrayBuilder; Lex.Identifier("i1"); Lex.Identifier("i2"); Lex.Assign;
            Lex.BeginNestedExpression; Lex.BeginNestedExpression; 
            Lex.Identifier("a"); Lex.LinkPoints; Lex.Identifier("b");
            Lex.EndNestedExpression; 
            Lex.Perpendicular; Lex.Fraction(1, 2); 
            Lex.BeginNestedExpression; Lex.Identifier("c"); Lex.LinkPoints; Lex.Identifier("d"); Lex.EndNestedExpression; 
            Lex.At; Lex.Fraction(67, 89); Lex.EndNestedExpression; 
            Lex.EndStackOps;]
        Assert.Equal<Lex.Token list>(lexed, expected);

    [<Fact>]
    let ``Lex an entire statement 2`` () =
        let statement = "<#> (! (a.b))"
        let lexed = Lex.lex statement |> List.map fst
        let expected = [Lex.CellBuilder; Lex.BeginNestedExpression; Lex.Draw; Lex.BeginNestedExpression;
            Lex.Identifier("a"); Lex.RecordAccess; Lex.Identifier("b"); Lex.EndNestedExpression; Lex.EndNestedExpression;]
        Assert.Equal<Lex.Token list>(lexed, expected);

