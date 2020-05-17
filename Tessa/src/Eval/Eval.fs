namespace Tessa.Eval

open Tessa.Language
open Tessa.Solve.Types
open Tessa.Parse
open Tessa.Lex
open Tessa.Util
open Tessa.Eval.Types
open Tessa.Eval.PrimitiveProcedures
open Tessa.Eval.Runtime

// todo: make sure Parse and Solve both return Result
module Eval = 
    module P = Parse
    module L = Language
    module S = SolveTypes
    module Lex = Lex
    open Util
    open EvalTypes
    open PrimitiveProcedures
    open Runtime

    type StackExecutionContext = {
        currentOp: OperationState;
        arguments: Exp list;
        runtime: Runtime;
        ret: Exp option;
    }

    type ExecutionContext = {
        // stackContext[i] has continuation stackContext[i + 1]
        continuations: StackExecutionContext list;
        currentContext: StackExecutionContext;
        reduction: Exp option;
    }

    let startingEnvironment : Environment= 
        Map.ofList [
        ("plus", (PrimitiveProcedure AddNumber));
        ("square", (PrimitiveProcedure Square));
        ("isosceles-right", (PrimitiveProcedure IsoscelesRight));
        ("c4-clockwise", (PrimitiveProcedure C4Clockwise));
        ("eval", (PrimitiveProcedure Eval));
        ("mirror", (PrimitiveProcedure Mirror));
        ("perturb", (PrimitiveProcedure Perturb));
        ("repeat-c4", (PrimitiveProcedure RepeatC4));
        ("hide-points", (PrimitiveProcedure HidePoints));
        ("true", (Bool true));
        ("false", (Bool false));
        ("_", (Quote(P.Expression(P.Identifier("_")))));
        ("_*", (Quote(P.Expression(P.Identifier("_*")))));
        ("tessa", (PrimitiveProcedure Tessa));]

    let startingDynamicEnvironment: DynamicEnvironment = 
        Map.ofList [(hidePointsVariable, (Bool false))]

    let emptyRuntime = {
        environment = startingEnvironment;
        dynamicEnvironment = Map.empty;
        labels = Map.empty;
        tessellations = [];
    }

    let emptyStackExecutionContext = {
        currentOp = Empty;
        arguments = [];
        runtime = emptyRuntime;
        ret = None;}

    let emptyExecutionContext = {
        continuations = [];
        currentContext = emptyStackExecutionContext;
        reduction = None;
    }

    let updateTop f context = 
        {context with currentContext = (f context.currentContext)}

    let liftToExecutionContext 
        (f: StackExecutionContext -> Result<StackExecutionContext, EvalError>) 
        : ExecutionContext -> Result<ExecutionContext, EvalError>  = fun stackContext ->
            result {
                let! newContext = f stackContext.currentContext
                return {stackContext with currentContext = newContext}
            }

    let acceptExpressionHelper exp context = 
        match context.currentOp with
            | EmptyAcceptNext -> 
                match exp with 
                | PrimitiveProcedure p -> Ok <| (context.arguments, Op (Primitive p))
                | LambdaExp lambda -> Ok <| (context.arguments, Op (LambdaOp lambda))
                | _ -> Error <| ApplyingNonFunction exp
            | _ -> Ok <| (exp :: context.arguments, context.currentOp)

    let acceptExpression exp context = 
        result {
            let! (newArgs, newOp) = acceptExpressionHelper exp context.currentContext
            return updateTop (fun t -> {t with arguments = newArgs; currentOp = newOp}) context
        }

    let acceptNextOp context = updateTop (fun t -> {t with currentOp = EmptyAcceptNext;}) context 

    let returnToLastContinuation ret initContext topRuntime =
            // return to the last continuation
        match List.tryHead initContext.continuations with 
        | None -> Error UnbalancedParenExtraClose 
        | Some continuation -> 
            // This is a "merge down" operation that concats draw but not environment
            let newRuntime = mergeDown topRuntime continuation.runtime 
            let updatedCurrentContext =  {continuation with arguments = tryCons ret continuation.arguments; runtime = newRuntime;}
            let newContext = {initContext with continuations = List.tail initContext.continuations; currentContext = updatedCurrentContext;}
            Ok newContext

    let findIdentifier execContext ident = 
        match Map.tryFind ident execContext.currentContext.runtime.environment with
            | None -> Error <| UndefinedVariable(ident, "If you meant to assign, assign to a symbol.")
            | Some exp -> Ok exp

    let findDynamicIdentifier execContext ident = 
        match Map.tryFind ident execContext.currentContext.runtime.dynamicEnvironment with
            | None -> Error <| UndefinedVariable(ident, "Did you bind the dynamic variable? If you meant to assign, assign to a symbol.")
            | Some exp -> Ok exp

    let rec evalWord context exp  =
        match exp with
        | P.Number n -> Ok <| Number n
        | P.Identifier ident -> findIdentifier context ident 
        | P.String s -> Ok <| String s
        | P.DynamicIdentifier ident -> findDynamicIdentifier context ident
        | P.PrimitiveProcedure p -> parsePrimitiveToEvalPrimitive p |> PrimitiveProcedure |> Ok
        | P.Quote q -> Ok <| Quote q

    type StackAction = 
        | BeginNewStack
        | ReturnNewStack
        | Expression of P.Word 
        | EndStack
        | ReduceAndPushOp of Lex.PrimitiveProcToken option

    let flattenParseStackCommands commands = 
        let rec flatten command = 
            match command with 
            // oof that append at the end is horribly inefficient
            | P.NewStack cs -> BeginNewStack :: (List.collect flatten cs) @ [ReturnNewStack]
            | P.Expression word -> [Expression word]
            | P.ReduceAndPushOp(x) -> [ReduceAndPushOp x]
            | P.EndStack -> [EndStack]
        List.collect flatten commands

    let newTopFrame context = 
        {emptyStackExecutionContext with runtime = context.currentContext.runtime}

    let pushNewTopFrame context frame = 
        let current = context.currentContext 
        {context with currentContext = frame; continuations = current :: context.continuations;}

    let reduceStackHelper eval context = 
        match context.currentOp with 
            | Empty -> Ok (List.tryHead context.arguments, context.arguments, context.currentOp, context.runtime)

            | EmptyAcceptNext -> Ok (List.tryHead context.arguments, context.arguments, context.currentOp, context.runtime) 

            | Op o -> 
                match o with 
                | Primitive p -> 
                    result {
                        let fn = lookupPrimitiveProcedure p eval
                        let! (applied, message) = fn (List.rev context.arguments) context.runtime
                        let newRuntime = 
                            match message with
                            | Some m -> handleMessage m context.runtime
                            | None -> context.runtime
                        return (Some applied, [applied], Empty, newRuntime)
                    }
                | LambdaOp (Lambda(env, parameters, body)) ->
                    result {
                        let arguments = List.rev context.arguments
                        return! 
                            if List.length arguments <> List.length parameters
                            then Error <| FunctionCallArgumentMismatch(arguments, parameters)
                            else result {
                                // Eval doesn't send us messages
                                let! (applied, _) = eval [Quote body] {context.runtime with environment = (Map.union (Map.ofList <| List.zip parameters arguments) env)}
                                return (Some applied, [applied], Empty, context.runtime)
                            }
                    }

    let reduceStack eval context = 
        result {
            let! (ret, args, op, runtime) = reduceStackHelper eval context.currentContext
            return updateTop (fun t -> {t with ret = ret; arguments = args; currentOp = op; runtime = runtime}) context
        }

    let firstPriorityOption newReduction lastResult = 
        match (newReduction, lastResult) with
        | (Some (x, nr), Some (y, lr)) -> 
            if x <= y then Some (x, nr) else Some (y , lr)
        | (Some (x, nr), None) -> Some (x, nr)
        | (None, Some (y, lr)) -> Some (y, lr)
        | (None, None) -> None 

    // Like a sauce in a pot all day!
    let tryReduceDown eval context = 
        let pop context = 
            let nextContinuation = List.tryHead context.continuations
            Option.map (fun c -> {context with currentContext = c; continuations = List.tail context.continuations}) nextContinuation

        let toOption result = 
            match result with
            | Ok o -> Some o
            | Error _ -> None

        let joinPriority p = 
            match p with 
            | None -> None 
            | Some (priority, exp) -> Option.map (fun e -> (priority, e)) exp

        // This is really a mess right now.
        // Lower priority is better; lowest priority is always the top frame.
        let rec go invPriority context  = 
            match context.continuations with 
            // If there are no continuations, it's easy -- reduce the top and only frame. This may or may not yield a value.
            | [] -> 
                let reduced = reduceStackHelper eval context.currentContext |> toOption |> Option.map (fun (ret, _, _, _) -> ret) |> Option.flatten
                Option.map (fun r -> (invPriority, r)) reduced

            // As an example for when we have continuations, let's use 
            // 2 :plus 3 :plus ('i = 
            // The partial evaluation should be 5, because we give up on ('i =
            // and go to the last continuation.
            | _::_ ->  
                // First we get the fallback by popping the top frame and recursively getting the result
                // as if the top frame never existed.
                let fallBack = pop context |> Option.map (go (invPriority + 1)) |> Option.flatten
                let withThisFrameResult = result {
                    // Then we reduce the top frame and push to the continuation and try reducing.
                    let! (ret, _, _, topRuntime) = reduceStackHelper eval context.currentContext 
                    let! returnedContext = returnToLastContinuation ret context topRuntime
                    return go invPriority returnedContext
                } 

                let withThisFrame = toOption withThisFrameResult |> Option.flatten
                // This lets us compare two things below:
                // The continuation reduced without this frame, and the continuation reduced with it.
                // If the introduction of this frame still yields a good result, we prefer that.
                // But otherwise, we'll go with the fallback (if it's None, we lose nothing; if it's
                // Some, that means the current frame is incomplete and introducing an error)
                firstPriorityOption withThisFrame fallBack

        go 0 context

    let evalStackCommand evalPrim initContext stackCommand = 
        match stackCommand with 
        | BeginNewStack -> 
            newTopFrame initContext |> pushNewTopFrame initContext |> Ok

        | Expression word -> 
            evalWord initContext word |> Result.bind (flip acceptExpression initContext)

        | EndStack -> 
            reduceStack evalPrim initContext |> Result.map (updateTop (fun t -> {t with arguments= []; currentOp = Empty;}))

        | ReduceAndPushOp(maybePrimitive) -> 
            // todo: reduce is possible
            match maybePrimitive with
            | Some primitive -> 
                result {
                    let! acceptingContext = initContext |> reduceStack evalPrim |> Result.map acceptNextOp 
                    let evalPrimitive = parsePrimitiveToEvalPrimitive primitive |> PrimitiveProcedure
                    return! acceptExpression evalPrimitive acceptingContext
                }
            | None -> initContext |> reduceStack evalPrim |> Result.map acceptNextOp // liftToExecutionContext (reduceStack >=> acceptNextOp) initContext

        | ReturnNewStack -> 
            result {
                let! (ret, _, _, topRuntime) = reduceStackHelper evalPrim initContext.currentContext
                return! returnToLastContinuation ret initContext topRuntime
            }

    let rec evalInEnv stackCommands runtime =
        let evalPrim arguments runtime = 
            match arguments with 
            | [(Quote(stackCommand))] -> 
                let evalResult = (evalInEnv [stackCommand] runtime)
                Option.cata 
                    Ok
                    (Option.cata Error (Error <| WrongArgumentsEval arguments) evalResult.error) 
                    (evalResult.value |> Option.map (fun v -> (v, evalResult.runtime)))
            | _ -> Error <| WrongArgumentsEval arguments

        let rec go context commands lastResult = 
            match commands with 
            | [] -> (lastResult, Ok context)
            | command :: rest -> 
                match evalStackCommand evalPrim context command with 
                | Ok newContext -> 
                    let newReduction = tryReduceDown evalPrim newContext
                    let newLastResult = firstPriorityOption newReduction lastResult
                    go newContext rest newLastResult // (firstOption (tryReduceDown newContext) lastResult)
                | Error e -> (lastResult, Error (context, e))

        let startingExecContext = {
            emptyExecutionContext with 
                currentContext = {
                    emptyStackExecutionContext with runtime = runtime}}

        let (opt, result) = go startingExecContext (flattenParseStackCommands stackCommands) None

        let partialResult = Option.map (fun (_, ret) -> ret) opt
        // Fold with "merge down"
        let makeRuntime r = r.continuations |> List.map (fun c -> c.runtime) |> List.fold mergeDown r.currentContext.runtime; 
        Result.cata
            (fun r -> {
                value = partialResult; 
                runtime = {makeRuntime r with environment = r.currentContext.runtime.environment};
                error = None;})
            (fun (context, e) -> {
                value = partialResult; 
                runtime = makeRuntime context;
                error = Some e;})
            result

    let eval stackCommands =
        evalInEnv stackCommands {emptyRuntime with environment = startingEnvironment}