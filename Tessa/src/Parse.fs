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
        // has optional assignment semantics also! more convenient.
        | CellBuild

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
        let tokenToPrimitive = function
            | Lex.ArrayBuilder -> Some ArrayBuilder
            | Lex.RecordBuilder -> Some RecordBuilder
            | Lex.LinkPoints -> Some LinkPoints
            | Lex.Perpendicular -> Some Perpendicular
            | Lex.Intersect -> Some Intersect
            | Lex.At -> Some At
            | Lex.ApplyOp -> Some ApplyOp
            | Lex.Assign -> Some Assign
            | Lex.Snip -> Some Snip
            | Lex.RecordAccess -> Some RecordAccess
            | Lex.Draw -> Some Draw
            | Lex.CellBuilder -> Some CellBuild
            | _ -> None
        let t = List.Cons >> Ok
        match tokens with
        | Lex.QuotePrimitive :: t :: rest ->
            match tokenToPrimitive t with
            | None -> Error {message = "Can't quote non-primitive.";}
            | Some p -> recurse (p |> PrimitiveProcedure |> Expression) rest
        | t :: rest ->
            match tokenToPrimitive t with
            | Some primitive -> recurse (Some primitive |> ReduceAndPushOp) rest
            | None -> 
                match t with
                | Lex.StackOp -> recurse (ReduceAndPushOp None) rest
                | Lex.EndStackOps -> recurse EndStack rest
                | Lex.BeginNestedExpression -> recurse BeginNewStack rest
                | Lex.EndNestedExpression -> recurse EndStack rest
                | Lex.Identifier i -> recurse (Expression <| Identifier i) rest
                | Lex.Fraction (numer, denom) -> recurse ((float numer) / (float denom) |> Number |> Expression) rest
                | _ -> Error {message = "Should be impossible to get here, all cases covered";}
        | [] -> Ok []
    
