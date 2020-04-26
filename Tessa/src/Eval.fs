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
        // | ArrayBuilder 
        // | RecordBuilder 
        // | LinkPoints 
        // | Perpendicular 
        // | Intersect 
        // | At 
        // | ApplyOp 
        // | Assign
        // | RecordAccess
        // | Snip
        // | Draw
        // | Lambda
        // // has optional assignment semantics also! more convenient.
        // | CellBuild
        // | Quote
        | AddNumber

    // todo: Need to pipe Lex pos into Parse so I can add positions here
    type EvalError =
        | UndefinedVariable of string 
        | AddingNonNumbers of Exp list
        | ApplyingNonFunction of Exp
        | UnbalancedParenExtraClose

    and Exp =
        | Number of float
        | Identifier of string
        | PrimitiveProcedure of PrimitiveProcedure
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

    type PrimitiveProcedureFn = (Exp list) -> (Exp list) -> Result<Exp, EvalError>

    // Primitive procedures
    // todo: many of these will have to reverse the lists first in order to sensibly apply
    let addNumber before after =
        let everything = before @ after
        let numbers = List.map toNumber everything
        let errs = errors numbers
        let oks = okays numbers
        if not (List.isEmpty errs) then Error <| AddingNonNumbers errs else List.sum oks |> Number |> Ok

    let lookupPrimitiveProcedure = function
        | AddNumber -> addNumber

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
            | Empty -> Ok {context with currentOp = Empty; beforeOp = []; afterOp = []}
            | EmptyAcceptNext -> Ok {context with currentOp = Empty; beforeOp = [];afterOp = []}
            | Op o -> 
                match o with 
                | Primitive p -> monad {
                    let fn = lookupPrimitiveProcedure p
                    let! applied = fn context.beforeOp context.afterOp
                    let newStack = {context with beforeOp = [applied]; afterOp = []; currentOp = Empty}
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
            | None -> Error <| UndefinedVariable ident
            | Some exp -> Ok exp

    let rec evalWord context exp  =
        match exp with
        | P.Number n -> Ok <| Number n
        | P.Identifier ident -> findIdentifier context ident 
            // | P.PrimitiveProcedure p -> 

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
                let ret = List.tryHead newStackContext.beforeOp
                return {context with currentContext = {emptyStackExecutionContext with ret = ret}}
            }
        | P.ReduceAndPushOp(maybePrimitive) -> 
            match maybePrimitive with
            | Some _ -> Ok context 
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
