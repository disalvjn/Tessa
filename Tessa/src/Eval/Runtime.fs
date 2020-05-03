namespace Tessa.Eval.Runtime

open Tessa.Language
open Tessa.Eval.Types
open Tessa.Util
open Tessa.Parse

// TODO: could make error handling in this significantly better by having recursive EvalErrors and building stack traces.
// Plus, pipe Lex positional info into Parse and use that in the evaluator.
module Runtime = 
    open EvalTypes
    let x = 1

    let mergeDraws = Map.unionWith (fun x y -> List.distinct <| x @ y)

    // type Runtime = {
    //     drawMap: DrawMap;
    //     environment: Environment;
    //     geoCanon: S.CanonicizerState;
    //     
    //     polygons: S.Polygon list;
    // }

    let handleMessage (message: EvaluatorMessage) (runtime: Runtime) : Runtime = 
        match message with
        | AugmentEnvironment e -> {runtime with environment = Map.union e runtime.environment} 
        | DrawGeo(key, draws) -> {runtime with drawMap = (mergeDraws (Map.add key [draws] Map.empty) runtime.drawMap);}

    let mergeDown topFrameRuntime continuationRuntime =
        {continuationRuntime with drawMap = mergeDraws topFrameRuntime.drawMap continuationRuntime.drawMap;}