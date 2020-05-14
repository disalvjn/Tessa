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

    let mergeDraws = Map.unionWith (fun x y -> List.distinct <| x @ y)


    let handleMessage (message: EvaluatorMessage) (runtime: Runtime) : Runtime = 
        match message with
        | AugmentEnvironment e -> {runtime with environment = Map.union e runtime.environment} 
        | DrawGeo(key, draws) -> 
            let newDrawMap = mergeDraws (Map.add (CellName key) [draws] Map.empty) runtime.drawMap
            // let polygons = Map.mapListMany geoExpsToPolygons newDrawMap
            // let polygonExps = List.map (Polygon >> GeoExp) polygons
            // let bindings = List.zip (List.map (fun (_, name, index) -> polygonName name index) polygons) polygonExps |> Map.ofList
            // let newEnvironment = Map.union bindings runtime.environment 
            // Create AbsolutePoints from centroids and name them and send to environment
            {runtime with drawMap = newDrawMap;}


            // {runtime with drawMap = ;}

    let mergeDown topFrameRuntime continuationRuntime =
        {continuationRuntime with 
            drawMap = mergeDraws topFrameRuntime.drawMap continuationRuntime.drawMap;
            dynamicEnvironment = Map.union topFrameRuntime.dynamicEnvironment continuationRuntime.dynamicEnvironment;}