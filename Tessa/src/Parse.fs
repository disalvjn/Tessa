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
                result {
                    let! (nested, restRest) = parseList rest
                    let command = NewStack nested
                    let! (parsedRest, restRestRest) = parseList restRest 
                    return (command :: parsedRest, restRestRest)
                }
            | rest ->  
                result {
                    let! (parsed, restOfRest) = parse rest
                    let! (parsedRest, restOfRestOfRest) = parseList restOfRest
                    return (tryCons parsed parsedRest, restOfRestOfRest)
                }

    and parse (tokens: Lex.Token list) : Result<(StackCommand option * Lex.Token list), ParseError> = 
        match tokens with
        | Lex.QuotePrimitive :: rest -> 
            result {
                let! (nextExpr, restOfRest) = parse rest
                return (Option.map (Quote >> Expression) nextExpr, restOfRest)
            }

        | Lex.StackOp :: rest -> 
            Ok (Some <| ReduceAndPushOp None, rest)// Ok ((ReduceAndPushOp None) |> Some, rest)

        | Lex.EndStackOps :: rest -> 
            Ok (Some EndStack, rest)

        | Lex.Identifier i :: rest -> 
            Ok (Identifier i |> Expression |> Some, rest)

        | Lex.Fraction (numer, denom) :: rest -> 
            Ok ((float numer) / (float denom) |> Number |> Expression |> Some, rest)

        | Lex.PrimitiveProc pp :: rest -> 
            Ok (tokenToPrimitive pp |> Some |> ReduceAndPushOp |> Some, rest)

        | Lex.WhiteSpace :: rest -> 
            Ok (NewStack [] |> Some, rest)

        | Lex.BeginNestedExpression :: rest -> 
            parseList rest |> Result.bind (fun (parsed, restOfRest) -> Ok (Some <| NewStack parsed, restOfRest))

        | Lex.EndNestedExpression :: rest -> 
            Error ExtraEndingParen

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
    
