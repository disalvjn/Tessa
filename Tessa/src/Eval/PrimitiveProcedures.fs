namespace Tessa.Eval.PrimitiveProcedures

open Tessa.Language
open Tessa.Eval.Types
open Tessa.Util
open Tessa.Parse
open Tessa.Lex

// TODO: could make error handling in this significantly better by having recursive EvalErrors and building stack traces.
// Plus, pipe Lex positional info into Parse and use that in the evaluator.
module PrimitiveProcedures = 
    open EvalTypes
    open Util
    module P = Parse
    module Lex = Lex
    module L = Language

    type PrimitiveProcedureFn = (Exp list) -> Runtime -> Result<Exp * EvaluatorMessage Option, EvalError>

    let parsePrimitiveToEvalPrimitive = function
        | Lex.Assign -> Assign
        | Lex.ArrayBuilder -> ArrayBuilder
        | Lex.RecordBuilder -> RecordBuilder
        | Lex.LinkPoints -> LinkPoints
        | Lex.Perpendicular -> Perpendicular
        | Lex.Intersect -> Intersect
        | Lex.At -> At
        | Lex.ApplyOp -> ApplyOp
        | Lex.Snip -> Snip
        | Lex.RecordAccess -> RecordAccess
        | Lex.Draw -> Draw
        | Lex.Is -> Is
        | Lex.Lambda -> MakeLambda
        | Lex.DynamicBind -> DynamicBind
        | Lex.DynamicBindDraw -> DynamicBindDraw

    let addNumber arguments env =
        let numbers = List.map toNumber arguments
        let errs = errors numbers
        let oks = okays numbers

        if not (List.isEmpty errs) 
        then Error <| AddingNonNumbers errs 
        else Ok(List.sum oks |> Number, None) 

    let extractSymbol = function 
        | Quote(P.Expression(P.Identifier i)) -> Ok i 
        | x -> Error <| NotASymbol x

    let assign arguments env = 
        match arguments with 
        | Quote(P.Expression(P.Identifier i)) :: [a] -> Ok(a, Map.add i a Map.empty |> AugmentEnvironment |> Some)
        | Array a :: [Array b] -> 
            if (List.length a) <> (List.length b)
            then Error <| ArrayAssignmentUnequalCardinalities(a, b)
            else 
                let asSymbols = Result.sequence <| List.map extractSymbol a
                asSymbols |> Result.bind (fun symbols -> Ok(Array b, List.zip symbols b |> List.fold (fun e (k, v) -> Map.add k v e) Map.empty |> AugmentEnvironment |> Some))
        | _ -> Error AssignError // todo: could make this a lot more specific

    let makeRecord arguments runtime =
        let env = runtime.environment
        let lookupThenTupTo lookupSym = 
            match Map.tryFind lookupSym env with
            | None -> Error <| UndefinedVariable(lookupSym, "Trying to make a record with field " 
                + lookupSym + "; no value specified, and failed to lookup symbol in environment.")
            | Some exp -> Ok (lookupSym, exp)

        let rec partition args = 
            let recurseIfOk (rest: Exp list) (resultVal: Result<string * Exp, EvalError>) = result {
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
        Result.map (fun record -> (Record record, None)) recordMap

    let recordAccess arguments env = 
        match arguments with
        | [(Record r); (Quote(P.Expression(P.Identifier i)))] -> 
            match Map.tryFind i r with 
            | None -> Error <| RecordAccessError(i, Some <| Record r)
            | Some v -> Ok (v, None)
        | _ -> Error <| RecordAccessError("There aren't two arguments, or they aren't records and symbols, or I don't know -- you messed up.", None)

    let arrayBuilder arguments env = Ok (Array arguments, None)

    let rec linkPoints arguments env = 
        match arguments with 
        // | [GeoExp(Polygon(centroid, _, _)); x] -> linkPoints [GeoExp(LPoint centroid); x] env
        // | [x; GeoExp(Polygon(centroid, _, _))] -> linkPoints [x;GeoExp(LPoint centroid)] env
        | [GeoExp(LPoint p); GeoExp(LPoint q)] as a -> Ok (L.Link (p, q) |> LSegment |> GeoExp, None)
        | [GeoExp(LPoint p); GeoExp(LSegment s)] -> Ok (L.ReverseChain(p, s) |> LSegment |> GeoExp, None)
        | [GeoExp(LSegment s); GeoExp(LPoint p)] -> Ok (L.Chain(s, p) |> LSegment |> GeoExp, None) 
        | [GeoExp(LSegment s); GeoExp(LSegment r)] -> Ok (L.Concat(s, r) |> LSegment |> GeoExp, None)
        | _ -> Error <| LinkingMoreThanTwoPointsOrSegments arguments 

    let asSegment x = 
        match x with
        | GeoExp (LSegment s) -> Ok s 
        | _ -> Error <| NotASegment x

    let asNumber x = 
        match x with 
        | Number n -> Ok n
        | _ -> Error <| NotANumber x

    let asPoint x =
        match x with 
        | GeoExp (LPoint p) -> Ok p 
        | _ -> Error <| NotAPoint x

    let asOp x =
        match x with 
        | LOperation o -> Ok o
        | _ -> Error <| NotAnOperation x

    let perpendicular arguments env =
        match arguments with 
        | [segment; position;] -> 
            result {
                let! p = asNumber position
                let! s = asSegment segment
                return (L.Line.Perpendicular(p, s) |> LLine |> GeoExp, None)
            }
        | [segment; position; endSegment;] ->
            result {
                let! p = asNumber position
                let! s = asSegment segment 
                let! es = asSegment endSegment
                return (L.Segment.Perpendicular(p, s, es) |> LSegment |> GeoExp, None)
            }
        | _ -> Error <| WrongArgumentsToPerpendicular arguments

    let at arguments env = 
        match arguments with
        | [segment; position;] ->
            result {
                let! p = asNumber position
                let! s = asSegment segment
                return (L.Point.OnSegment(L.PointOnSegment(p, s)) |> LPoint |> GeoExp, None)
            }
        | _ -> Error <| WrongArgumentsToAt arguments

    let rotation arguments env direction angle =
        match arguments with 
        | [point] -> asPoint point |> Result.bind (fun p -> Ok (L.Rotate(direction, angle, p) |> LOperation, None))
        | _ -> Error <| WrongArgumentsToRotation arguments

    let c4Clockwise arguments env =
        rotation arguments env L.Clockwise L.C4

    let applyOp arguments env = 
        match arguments with 
        | [point; op;] -> 
            result {
                let! p = asPoint point 
                let! o = asOp op
                return (L.Operated(p, o) |> LPoint |> GeoExp, None)
            }
        | _ -> Error <| WrongArgumentsToApplyOp arguments

    let snip arguments env =
        match arguments with 
        | [segment; cutAt;] ->
            result {
                let! s = asSegment segment 
                let! c = asSegment cutAt 
                return (L.Segment.Snipped(s, c) |> LSegment |> GeoExp, None)
            }
        | _ -> Error <| WrongArgumentsToSnip arguments

    let intersect arguments env = 
        match arguments with 
        | [GeoExp(LLine l); GeoExp(LLine m)] -> Ok (L.Point.Intersection(l, m) |> LPoint |> GeoExp, None)
        | [GeoExp(LLine l); GeoExp(LSegment s)] -> Ok (L.Point.Intersection(l, L.asLine s) |> LPoint |> GeoExp, None)
        | [GeoExp(LSegment s); GeoExp(LLine l)] -> Ok (L.Point.Intersection(L.asLine s, l) |> LPoint |> GeoExp, None)
        | [GeoExp(LSegment s); GeoExp(LSegment r)] -> Ok (L.Point.Intersection(L.asLine s, L.asLine r) |> LPoint |> GeoExp, None)
        | _ -> Error <| WrongArgumentsToIntersect arguments

    let toPointArray points = 
        points
        |> List.map (LPoint >> GeoExp)
        |> Array
        |> flip tuple2 None
        |> Ok

    let makeSquare arguments env = 
        toPointArray [L.Absolute(0.0, 1.0); L.Absolute(1.0, 1.0); L.Absolute(1.0,0.0); L.Absolute(0.0,0.0);]

    let makeIsoscelesRight arguments env = 
        let x = (sqrt 2.0) / 2.0
        toPointArray [L.Absolute(0.0, 0.0); L.Absolute (0.0, x); L.Absolute (x, x);]

    let makeLambda arguments runtime = 
        let env = runtime.environment
        result {
            let (parameters, body) = List.splitAt (List.length arguments - 1) arguments
            let! paramsAsSymbols = List.map extractSymbol parameters |> Result.sequence |> Result.mapError LambdaArgumentNotSymbol
            return! 
                match body with 
                | [Quote(stackCommand)] -> Ok (LambdaExp <| Lambda(env, paramsAsSymbols, stackCommand), None)
                | _ -> Error <| LambdaBodyNotExpression body
        }

    let dynamicBind eval arguments runtime =
        match arguments with
        | [Quote(P.Expression(P.DynamicIdentifier(sym))); exp; Quote(stackCommand)] -> 
            eval [Quote stackCommand] {runtime with dynamicEnvironment = Map.add sym exp runtime.dynamicEnvironment}
        | _ -> Error <| WrongArgumentsDynamicBind arguments

    let dynamicBindDraw eval arguments runtime = 
        match arguments with 
        | [Quote(stackCommand)] -> 
            result {
                let! (_, runtime) = eval [Quote stackCommand] {runtime with dynamicEnvironment = Map.add "&!" (L.Primary [] |> LCell |> GeoExp) runtime.dynamicEnvironment;}
                let valFromRuntime = Map.find "&!" runtime.dynamicEnvironment
                return (valFromRuntime, None)
            }
        | _ -> Error <| WrongArgumentsDynamicBindDraw arguments

    let draw arguments runtime = 
        match arguments with 
        | [GeoExp (LSegment seg)] ->
            match Map.tryFind "&!" runtime.dynamicEnvironment with 
            | Some(GeoExp(LCell (L.Primary segments))) ->
                let newCell = L.Primary (seg :: segments) |> LCell |> GeoExp
                Ok (newCell, Map.ofList [("&!", newCell)] |> AugmentDynamicEnvironment |> Some)
            | Some x -> Error <| DrawDynamicVarImproperlyBound x
            | None -> Error <| DrawDynamicVarUnbound
        | _ -> Error <| WrongArgumentsToDraw arguments 

    // let draw arguments env =
    //     match arguments with
    //     | [GeoExp shape as gs; Quote(P.Expression(P.Identifier key))] -> Ok (gs, Some <| DrawGeo (key, shape))
    //     | [GeoExp shape as gs] -> Ok(gs, Some <| DrawGeo ("p", shape))
    //     | _ -> Error <| WrongArgumentsToDraw arguments


    let lookupPrimitiveProcedure (p: PrimitiveProcedure) (eval: Exp list -> Runtime -> Result<(Exp* Runtime), EvalError>) : PrimitiveProcedureFn = 
        let evalJustResult = (fun arguments runtime -> eval arguments runtime |> Result.map (fun (result, runtime) -> (result, None)))
        match p with
        | AddNumber -> addNumber
        | Assign -> assign
        | RecordBuilder -> makeRecord
        | RecordAccess -> recordAccess
        | ArrayBuilder -> arrayBuilder
        | Square -> makeSquare
        | LinkPoints -> linkPoints
        | Perpendicular -> perpendicular
        | At -> at
        | C4Clockwise -> c4Clockwise
        | ApplyOp -> applyOp
        | Snip -> snip
        | Intersect -> intersect
        | Draw -> draw
        | IsoscelesRight -> makeIsoscelesRight
        | Is -> (fun arguments env -> assign (List.rev arguments) env)
        | Eval -> evalJustResult
        | MakeLambda -> makeLambda
        | DynamicBind -> dynamicBind evalJustResult
        | DynamicBindDraw -> dynamicBindDraw eval 
