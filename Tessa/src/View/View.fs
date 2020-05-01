namespace Tessa.View

open Tessa.View.Types
open Tessa.Eval
open Tessa.Eval.Types
open Tessa.Solve
open FSharpPlus
open Tessa.Util

module View = 
    open ViewTypes
    module E = Eval
    module E = EvalTypes
    module S = Solve

    let isGeoExp = function 
        | E.GeoExp g -> Some g
        | _ -> None

    let solveGeoExp exp (solve: S.Solver) = 
        // todo: refactor to prevent LOperation from being here.
        // todo: implement for polygons
        match exp with 
        | E.LLine line -> solve.line line |>> ViewLine
        | E.LSegment s -> solve.segment s |>> ViewSegment 
        | E.LPoint p -> solve.point p |>> ViewPoint

    let viewsFromEvalResult (evalResult: E.EvalResult) (solve: S.Solver) : View list =
        // todo: long term, solving shouldn't happen in here. There will be an extra layer that computes polygons,
        // applies tessellations, determines indexes. 

        let (env, draw, result) = (evalResult.environment, evalResult.draw, evalResult.value)
        // we should be able to handle partial errors
        let fromEnv = env |> Map.filterSome isGeoExp |> Map.mapList (fun label shape -> (Preview lable, solveGeoExp shape solve))

        // env -> filter for GeoExp -> (k, v) -> {mode= PReview k; view= solveGeoExp v}
        // let fromEnv = Map.filter 
        // preview everything in env and result
        // draw everything in draw
        // make sure intersections between draw and result/env are shown only once

