namespace Tessa.Eval

open Tessa.Language
open Tessa.Solve
open Tessa.Parse
open Tessa.Util
open FSharpPlus

// todo: make sure Parse and Solve both return Result
module Eval = 
    module P = Parse
    module L = Language
    module S = Solve
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
        | AddNumber

    // todo: Need to pipe Lex pos into Parse so I can add positions here
    type EvalError =
        | UndefinedVariable of var: string * message: string
        | AddingNonNumbers of Exp list
        | ApplyingNonFunction of Exp
        | UnbalancedParenExtraClose
        | AssignError

    and Exp =
        | Number of float
        | Identifier of string
        | PrimitiveProcedure of PrimitiveProcedure
        | Quote of P.StackCommand
        // | Lambda
        // | Record
        // | LanguageExp of LanguageExp
        // Language Unsolved

    and LanguageExp = 
        | LPoint of L.Point
        | LSegment of L.Segment
        | LLine of L.Line
        | LOperation of L.Operation
        | LPolygon of L.Polygon

    and SolveExp = 
        | SPoint of S.Point
        | SSegment of S.Segment
        | SLine of S.Line

    let toNumber exp = 
        match exp with 
        | Number n -> Ok n 
        | other -> Error other 

    type Operation =
        | Primitive of PrimitiveProcedure
        // | Fun

    type OperationState = 
        | Empty 
        | EmptyAcceptNext
        | Op of Operation

    type StackExecutionContext = {
        currentOp: OperationState;
        beforeOp: Exp list;
        afterOp: Exp list;
        environment: Map<string, Exp>;
        ret: Exp option;
        // doesn't work because of primitive functions
        // subExpressions: Map<Exp, Exp>;
        // continuation implicitly stored in list
    }

    type ExecutionContext = {
        // stackContext[i] has continuation stackContext[i + 1]
        continuations: StackExecutionContext list;
        currentContext: StackExecutionContext
        solveContext: S.SolveContext;
        // have a Solve function to seemlessly evaluate 
    }

    type Environment = Map<string, Exp>
    type PrimitiveProcedureFn = (Exp list) -> (Exp list) -> Result<Exp * Environment, EvalError>

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

    // Primitive procedures
    // todo: many of these will have to reverse the lists first in order to sensibly apply
    let addNumber before after env =
        let everything = before @ after
        let numbers = List.map toNumber everything
        let errs = errors numbers
        let oks = okays numbers

        if not (List.isEmpty errs) 
        then Error <| AddingNonNumbers errs 
        else Ok(List.sum oks |> Number, env) 

    let assign before after env = 
        match (before, after) with 
        | ([Quote(P.Expression(P.Identifier i))], [a]) -> Ok(a, Map.add i a env)
        | _ -> Error AssignError // todo: could make this a lot more specific

    let lookupPrimitiveProcedure = function
        | AddNumber -> addNumber
        | Assign -> assign

    let startingEnvironment = 
        Map.empty 
        |> Map.add "plus" (PrimitiveProcedure AddNumber)

    let emptyStackExecutionContext = {
        currentOp = Empty;
        beforeOp = [];
        afterOp = [];
        environment = startingEnvironment;
        ret = None;}
        //subExpressions = Map.empty}

    let liftToExecutionContext 
        (f: StackExecutionContext -> Result<StackExecutionContext, EvalError>) 
        : ExecutionContext -> Result<ExecutionContext, EvalError>  = fun stackContext ->
            monad {
                let! newContext = f stackContext.currentContext
                return {stackContext with currentContext = newContext}
            }

    let acceptExpression exp context = 
        match context.currentOp with
            | Empty -> Ok <| {context with beforeOp = exp :: context.beforeOp}
            | EmptyAcceptNext -> 
                match exp with 
                | PrimitiveProcedure p -> Ok <| {context with currentOp = Op (Primitive p)}
                | _ -> Error <| ApplyingNonFunction exp
            | Op _ -> Ok <| {context with afterOp = exp :: context.afterOp}

    let acceptNextOp context = Ok <| {context with currentOp = EmptyAcceptNext}

    let reduceStack context = 
        match context.currentOp with 
            | Empty -> Ok {context with currentOp = Empty; beforeOp = []; afterOp = []; ret = List.tryHead context.beforeOp;}
            | EmptyAcceptNext -> Ok {context with currentOp = Empty; beforeOp = [];afterOp = []; ret = List.tryHead context.beforeOp;}
            | Op o -> 
                match o with 
                | Primitive p -> monad {
                    let fn = lookupPrimitiveProcedure p
                    let! (applied, newEnv) = fn context.beforeOp context.afterOp context.environment
                    let newStack = {context with beforeOp = [applied]; afterOp = []; currentOp = Empty; environment = newEnv; ret = Some applied;}
                    return newStack
                }

    let emptyExecutionContext = {
        continuations = [];
        currentContext = emptyStackExecutionContext;
        solveContext = S.emptySolveContext;
    }

    // let applyPrimitive context (primitiveProcedure: PrimitiveProcedureFn) = monad {
    //     let! newStackExecutionContext = primitiveProcedure context.currentContext.beforeOp context.currentContext.afterOp
    //     {context with currentContext = newStackExecutionContext}
    // }

    let findIdentifier execContext ident = 
        match Map.tryFind ident execContext.currentContext.environment with
            | None -> Error <| UndefinedVariable(ident, "If you meant to assign, assign to a symbol.")
            | Some exp -> Ok exp

    let rec evalWord context exp  =
        match exp with
        | P.Number n -> Ok <| Number n
        | P.Identifier ident -> findIdentifier context ident 
        | P.PrimitiveProcedure p -> parsePrimitiveToEvalPrimitive p |> PrimitiveProcedure |> Ok
        | P.Quote q -> Ok <| Quote q

    let evalStackCommand context stackCommand = 
        match stackCommand with 
        // 
        | P.BeginNewStack -> 
            let current = context.currentContext
            let top = {emptyStackExecutionContext with environment = current.environment}
            Ok <| {context with currentContext = top; continuations = current :: context.continuations;}
        | P.Expression word -> evalWord context word >>= (fun e -> (liftToExecutionContext (acceptExpression e) context))
        // continuation pushing . We have a return, now we just need to push it to the before op before us
        | P.EndStack -> 
            monad {
                let! newStackContext = reduceStack context.currentContext
                return {context with currentContext = {newStackContext with beforeOp = []; afterOp = []; currentOp = Empty;}}
            }
        | P.ReduceAndPushOp(maybePrimitive) -> 
            match maybePrimitive with
            | Some primitive -> 
                monad {
                    let! acceptingContext = liftToExecutionContext acceptNextOp context
                    let evalPrimitive = parsePrimitiveToEvalPrimitive primitive |> PrimitiveProcedure
                    return! (liftToExecutionContext (acceptExpression evalPrimitive) acceptingContext)
                }
            | None -> liftToExecutionContext acceptNextOp context
        | P.ReturnNewStack -> 
            monad {
                let! newStackContext = reduceStack context.currentContext
                let ret = List.tryHead newStackContext.beforeOp
                let returnTo = List.tryHead context.continuations 
                let newContext = 
                    match returnTo with
                    | None -> Error UnbalancedParenExtraClose 
                    | Some continuation -> 
                        let updatedCurrentContext =  {continuation with beforeOp = tryCons ret continuation.beforeOp}
                        let newContext = {context with continuations = List.tail context.continuations; currentContext = updatedCurrentContext}
                        Ok newContext
                return! newContext
            }

    let eval stackCommands =
        let rec go context commands = 
            match commands with 
            | [] -> Ok context
            | command :: rest -> monad {
                let! newContext = evalStackCommand context command
                return! go newContext rest
            }
        go emptyExecutionContext stackCommands

            


    // and StackCommand =
    //     | ReduceAndPushOp of PrimitiveProcedure option
    //     | BeginNewStack
    //     | EndStack
    //     | Expression of Word



    // let primitiveLookup : Map<string, (Exp list -> Exp)> =
    //     [("add", )]
