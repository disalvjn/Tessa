namespace Tessa.Parse
open Tessa.Lex
open Tessa.Util
open FSharpPlus

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
        // has optional assignment semantics also! more convenient. e.g. (<#> ( a + b !))
        | CellBuild

    type Word = 
        | Identifier of string
        | Number of float
        | PrimitiveProcedure of PrimitiveProcedure
        | Quote of StackCommand

    and StackCommand =
        | ReduceAndPushOp of PrimitiveProcedure option
        | NewStack of StackCommand list
        | EndStack
        | Expression of Word

    type ParseError = 
        | ExtraEndingParen

    let rec parseList allTokens = 
        match allTokens with 
            | [] -> Ok ([], [])
            | Lex.EndNestedExpression :: rest -> Ok([], rest)
            | Lex.BeginNestedExpression :: rest -> 
                monad {
                    let! (nested, restRest) = parseList rest
                    let command = NewStack nested
                    let! (parsedRest, restRestRest) = parseList restRest 
                    return (command :: parsedRest, restRestRest)
                }
            | rest ->  
                monad {
                    let! (parsed, restOfRest) = parse rest
                    let! (parsedRest, restOfRestOfRest) = parseList restOfRest
                    return (tryCons parsed parsedRest, restOfRestOfRest)
                }

    and parse (tokens: Lex.Token list) : Result<(StackCommand option * Lex.Token list), ParseError> = 
        match tokens with
        | Lex.QuotePrimitive :: rest -> 
            monad {
                let! (nextExpr, restOfRest) = parse rest
                return (Option.map (Quote >> Expression) nextExpr, restOfRest)
            }
        | t :: rest -> 
            match t with
            | Lex.StackOp -> Ok (Some <| ReduceAndPushOp None, rest)// Ok ((ReduceAndPushOp None) |> Some, rest)
            | Lex.EndStackOps -> Ok (Some EndStack, rest)
            | Lex.Identifier i -> Ok (Identifier i |> Expression |> Some, rest)
            | Lex.Fraction (numer, denom) -> Ok ((float numer) / (float denom) |> Number |> Expression |> Some, rest)
            | Lex.PrimitiveProc pp -> Ok (tokenToPrimitive pp |> Some |> ReduceAndPushOp |> Some, rest)
            | Lex.WhiteSpace -> Ok (NewStack [] |> Some, rest)
            | Lex.BeginNestedExpression -> parseList rest >>= (fun (parsed, restOfRest) -> Ok (Some <| NewStack parsed, restOfRest))
            | Lex.EndNestedExpression -> Error ExtraEndingParen
        | [] -> Ok (None, [])

    and tokenToPrimitive (procToken : Lex.PrimitiveProcToken) : PrimitiveProcedure = 
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
    
