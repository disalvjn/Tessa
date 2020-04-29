namespace Tessa.Eval.PrimitiveProcedures

open Tessa.Eval.Types
open Tessa.Util
open Tessa.Parse
open FSharpPlus

module PrimitiveProcedures = 
    open EvalTypes
    open Util
    module P = Parse

    type Environment = Map<string, Exp>
    type PrimitiveProcedureFn = (Exp list) -> Result<Exp * Environment, EvalError>

    let parsePrimitiveToEvalPrimitive = function
        | P.Assign -> Assign
        | P.ArrayBuilder -> ArrayBuilder
        | P.RecordBuilder -> RecordBuilder
        | P.LinkPoints -> LinkPoints
        | P.Perpendicular -> Perpendicular
        | P.Intersect -> Intersect
        | P.At -> At
        | P.ApplyOp -> ApplyOp
        | P.Snip -> Snip
        | P.RecordAccess -> RecordAccess
        | P.Draw -> Draw
        | P.Lambda -> Lambda
        | P.CellBuild -> CellBuild

    let addNumber arguments env =
        let numbers = List.map toNumber arguments
        let errs = errors numbers
        let oks = okays numbers

        if not (List.isEmpty errs) 
        then Error <| AddingNonNumbers errs 
        else Ok(List.sum oks |> Number, env) 

    let extractSymbol = function 
        | Quote(P.Expression(P.Identifier i)) -> Ok i 
        | x -> Error <| NotASymbol x

    let assign arguments env = 
        match arguments with 
        | Quote(P.Expression(P.Identifier i)) :: [a] -> Ok(a, Map.add i a env)
        | Array a :: [Array b] -> 
            if (List.length a) <> (List.length b)
            then Error <| ArrayAssignmentUnequalCardinalities(a, b)
            else 
                let asSymbols = sequence <| map extractSymbol a
                asSymbols >>= (fun symbols -> Ok(Array b, List.zip symbols b |> List.fold (fun e (k, v) -> Map.add k v e) env))
        | _ -> Error AssignError // todo: could make this a lot more specific

    let makeRecord arguments env =
        let lookupThenTupTo lookupSym = 
            match Map.tryFind lookupSym env with
            | None -> Error <| UndefinedVariable(lookupSym, "Trying to make a record with field " 
                + lookupSym + "; no value specified, and failed to lookup symbol in environment.")
            | Some exp -> Ok (lookupSym, exp)

        let rec partition args = 
            let recurseIfOk (rest: Exp list) (resultVal: Result<string * Exp, EvalError>) = monad {
                let! trueResult = resultVal 
                let! restResult = partition rest 
                return trueResult :: restResult
            }
            match args with 
            | [] -> Ok []
            | [Quote(P.Expression(P.Identifier i1))] -> lookupThenTupTo i1 |> recurseIfOk []
            | Quote(P.Expression(P.Identifier i1)) :: (Quote(P.Expression(P.Identifier i2)) as q2) :: rest -> 
                lookupThenTupTo i1 |> recurseIfOk (q2 :: rest)
            | Quote(P.Expression(P.Identifier i1)) :: x :: rest -> Ok (i1, x) |> recurseIfOk rest
            | _ -> Error RecordBuildingError
        
        let recordMap = Result.map listToMap <| partition arguments
        Result.map (fun record -> (Record record, env)) recordMap

    let recordAccess arguments env = 
        match arguments with
        | [(Record r); (Quote(P.Expression(P.Identifier i)))] -> 
            match Map.tryFind i r with 
            | None -> Error <| RecordAccessError(i, Some <| Record r)
            | Some v -> Ok (v, env)
        | _ -> Error <| RecordAccessError("There aren't two arguments, or they aren't records and symbols, or I don't know -- you messed up.", None)

    let arrayBuilder arguments env = Ok (Array arguments, env)


    let lookupPrimitiveProcedure = function
        | AddNumber -> addNumber
        | Assign -> assign
        | RecordBuilder -> makeRecord
        | RecordAccess -> recordAccess
        | ArrayBuilder -> arrayBuilder

