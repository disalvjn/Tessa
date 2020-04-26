namespace Tessa.Parse
open Tessa.Lex
open Tessa.Util

module Parse = 
    module Lex = Lex
    open Util

    type PrimitiveProcedure = 
        | ArrayBuilder 
        | RecordBuilder 
        | LinkPoints 
        | Perpendicular 
        | Intersect 
        | At 
        | ApplyOp 
        | Assign
        | RecordAccess
        | Snip
        | Draw
        | Lambda
        // has optional assignment semantics also! more convenient.
        | CellBuild
        | Quote

    type Word = 
        | Identifier of string
        | Number of float
        | PrimitiveProcedure of PrimitiveProcedure

    and StackCommand =
        | ReduceAndPushOp of PrimitiveProcedure option
        | BeginNewStack
        | EndStack
        | Expression of Word

    type ParseError = {message: string}

    let rec parse tokens =
        let recurse this rest = 
            Result.bind (Ok << cons this) (parse rest)
        let tokenToPrimitive (procToken : Lex.PrimitiveProcToken) : PrimitiveProcedure = 
            match procToken with
            | Lex.ArrayBuilder -> ArrayBuilder
            | Lex.RecordBuilder -> RecordBuilder
            | Lex.LinkPoints -> LinkPoints
            | Lex.Perpendicular -> Perpendicular
            | Lex.Intersect -> Intersect
            | Lex.At -> At
            | Lex.ApplyOp -> ApplyOp
            | Lex.Assign -> Assign
            | Lex.Snip -> Snip
            | Lex.RecordAccess -> RecordAccess
            | Lex.Draw -> Draw
            | Lex.Lambda -> Lambda
            | Lex.CellBuilder -> CellBuild
        let t = List.Cons >> Ok
        match tokens with
        | Lex.QuotePrimitive :: (Lex.PrimitiveProc t) :: rest ->
            recurse ((tokenToPrimitive t) |> PrimitiveProcedure |> Expression) rest
        | t :: rest ->
            // match tokenToPrimitive t with
            // | Some primitive -> recurse (Some primitive |> ReduceAndPushOp) rest
            // | None -> 
            match t with
            | Lex.StackOp -> recurse (ReduceAndPushOp None) rest
            | Lex.EndStackOps -> recurse EndStack rest
            | Lex.BeginNestedExpression -> recurse BeginNewStack rest
            | Lex.EndNestedExpression -> recurse EndStack rest
            | Lex.Identifier i -> recurse (Expression <| Identifier i) rest
            | Lex.Fraction (numer, denom) -> recurse ((float numer) / (float denom) |> Number |> Expression) rest
            | Lex.PrimitiveProc pp -> recurse (tokenToPrimitive pp |> PrimitiveProcedure |> Expression) rest
            | Lex.QuotePrimitive -> recurse (Quote |> PrimitiveProcedure |> Expression) rest
        | [] -> Ok []
    
