namespace Tessa.Eval.Runtime
open Tessa.Language
open Tessa.Eval.Types
open Tessa.Util
open Tessa.Parse
open Tessa.Solve.Shapes
open Tessa.Solve.Polygons
open Tessa.Util

// TODO: could make error handling in this significantly better by having recursive EvalErrors and building stack traces.
// Plus, pipe Lex positional info into Parse and use that in the evaluator.
module Runtime = 
    open EvalTypes
    module L = Language
    module S = SolveShapes
    module P = SolvePolygons
    open Util
    let x = 1


    let handleMessage (message: EvaluatorMessage) (runtime: Runtime) : Runtime = 
        match message with
        | AugmentEnvironment e -> 
            let pointLabels = Map.mapList (fun k v -> asPoint v |> Result.map (fun p -> (k, p))) e |> okays
            {runtime with 
                environment = Map.union e runtime.environment;
                labels = Map.union runtime.labels (Map.ofList pointLabels);} 
        | AugmentDynamicEnvironment e -> {runtime with dynamicEnvironment = Map.union e runtime.dynamicEnvironment} 
        | AugmentEnvAndLabels e -> 
            {runtime with 
                environment = Map.union (Map.map (fun _ v -> v |> LPoint |> GeoExp) e) runtime.environment;
                labels = Map.union e runtime.labels}

            // {runtime with drawMap = ;}

    let mergeDown topFrameRuntime continuationRuntime =
        {continuationRuntime with 
            dynamicEnvironment = Map.union topFrameRuntime.dynamicEnvironment continuationRuntime.dynamicEnvironment;
            labels = Map.union topFrameRuntime.labels continuationRuntime.labels;}