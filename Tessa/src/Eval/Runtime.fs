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
        | AugmentEnvironment e -> {runtime with environment = Map.union e runtime.environment} 
        | AugmentDynamicEnvironment e -> {runtime with dynamicEnvironment = Map.union e runtime.dynamicEnvironment} 

            // {runtime with drawMap = ;}

    let mergeDown topFrameRuntime continuationRuntime =
        {continuationRuntime with 
            dynamicEnvironment = Map.union topFrameRuntime.dynamicEnvironment continuationRuntime.dynamicEnvironment;}