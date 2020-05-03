namespace Tessa.View

open Tessa.View.Types
open Tessa.Eval
open Tessa.Eval.Types
open Tessa.Solve
open Tessa.Language
open Tessa.Util

module View = 
    open ViewTypes
    module E = EvalTypes
    module S = Solve
    module L = Language
    open Util

    let isGeoExp = function 
        | E.GeoExp g -> Some g
        | _ -> None

    let asAbsolutePoint = function 
        | E.LPoint (L.Absolute(x, y)) -> Some (x, y)
        | _ -> None

    let solveGeoExp (solve: S.Solver)  exp = 
        // todo: refactor to prevent LOperation from being here.
        // todo: implement for polygons
        match exp with 
        | E.LLine line -> solve.line line |> Result.map ViewLine
        | E.LSegment s -> solve.segment s |> Result.map ViewSegment 
        | E.LPoint p -> solve.point p |> Result.map ViewPoint

    let viewsFromEvalResult (evalResult: E.EvalResult) (solve: S.Solver) 
        : View list * (ViewMode * S.SolveError) list =
        let rec resultPartition = function 
            | [] -> ([], [])
            | (viewMode, Ok solved) :: rest -> 
                let (oks, errs) = resultPartition rest 
                ((viewMode, solved) :: oks, errs)
            | (viewMode, Error err) :: rest ->
                let (oks, errs) = resultPartition rest
                (oks, (viewMode, err) :: errs)
        
        let toView (mode, shape) = {viewShape = shape; viewMode = mode;}

        // todo: long term, solving shouldn't happen in here. There will be an extra layer that computes polygons,
        // applies tessellations, determines indexes. 

        let (env, draw, result) = (evalResult.runtime.environment, evalResult.runtime.drawMap, evalResult.value)
        // we should be able to handle partial errors
        let (fromEnvOks, fromEnvErrors) = 
            env 
            |> Map.filterSome isGeoExp 
            |> Map.mapList (fun label shape -> (Preview label, solveGeoExp solve shape)) 
            |> resultPartition

        let (fromDrawOks, fromDrawErrors) = 
            draw 
            |> Map.mapListMany (fun category shapes -> List.map (fun shape -> (Drawn category, solveGeoExp solve shape)) shapes) 
            |> resultPartition

        let fromResult = result |> Option.bind isGeoExp |> Option.map (fun s -> [(Preview "Current Exp", solveGeoExp solve s)])

        let (retOkays, retErrs) = (List.map toView <| fromEnvOks @ fromDrawOks, fromEnvErrors @ fromDrawErrors)

        // todo: make sure intersections between draw and result/env are shown only once
        match Option.map resultPartition fromResult with 
            | Some(okays, errs) -> (List.map toView okays @ retOkays, errs @ retErrs)
            | None -> (retOkays, retErrs)

    type PointTransform = {
        xScale: float;
        yScale: float;
        xTrans: float;
        yTrans: float;
        xMin: float;
        xMax: float;
        yMin: float;
        yMax: float;
        absoluteXMax: float; 
        absoluteYMax: float;
    }

    let extractAbsolutes (evalResult: E.EvalResult) = 
        let geometrics = 
            tryCons (Option.bind isGeoExp evalResult.value) <|
            ((evalResult.runtime.environment |> Map.filterSome isGeoExp |> Map.values |> Seq.toList)
            @ (List.concat <| Map.values evalResult.runtime.drawMap))

        List.map asAbsolutePoint geometrics |> somes

    let pointTranslation targets absolutePoints = 
        let bounds points = 
            let minX = List.minBy fst points |> fst
            let maxX = List.maxBy fst points |> fst
            let minY = List.minBy snd points |> snd
            let maxY = List.maxBy snd points |> snd
            (minX, maxX, minY, maxY)

        // todo: if just 0 or 1 points or all colinear ugh
        // we need three non-colinear points for a plane.
        let (minX, maxX, minY, maxY) = bounds absolutePoints
        let xScale = float targets.boundingWidth / (maxX - minX)
        let yScale = float targets.boundingHeight / (maxY - minY)
        let scale (x, y) = (xScale * x,  yScale * y)

        let newAbsolutes = List.map scale absolutePoints 
        let (newMinX, newMaxX, newMinY, newMaxY) = bounds newAbsolutes
        let xTrans = (fst targets.topLeft) - newMinX
        let yTrans = (snd targets.topLeft) - newMinY

        {xScale = xScale; yScale = yScale; xTrans = xTrans; yTrans = yTrans;
        xMin = newMinX; xMax = newMaxX; yMin = newMinY; yMax = newMaxY;
        absoluteXMax = targets.xMax; absoluteYMax = targets.yMax;}

    let applyPointTransform trans (point: S.Point) = 
        {x = trans.xScale * point.x + trans.xTrans; y = trans.yScale * point.y + trans.yTrans;}
    
    let applySegmentTransform trans (seg: S.Segment) = 
        match seg with 
        | S.Straight(orig, dest) -> {orig = applyPointTransform trans orig; dest = applyPointTransform trans dest}
    
    let applyLineTransform trans (line: S.Line) =   
        match line with 
        | S.Vertical(x) ->
            let newX = trans.xScale * x + trans.xTrans 
            {orig = {x = newX; y = 0.0;}; dest = {x = newX; y = trans.absoluteYMax;}}
        | S.Sloped(point, m) ->
            let newPoint = applyPointTransform trans point 
            let y x =  m * (x - newPoint.x) + newPoint.y
            // todo: need to solve for x that has max y
            {orig = {x = trans.xMin; y = y 0.0;}; dest = {x = trans.xMax; y = y trans.yMax;}}


    // todo: Somehow need a bounding box 
    let toDrawable trans view = 
        let poptions label style = {color = "#004080"; label = label; pointStyle = style}
        let soptions label style = {color = "#004080"; label = label; lineStyle = style; lineWidth = Medium; accentuatePoints = false}
        match (view.viewShape, view.viewMode) with 
        | (ViewPoint p, Preview label) -> [DrawPoint(applyPointTransform trans p, poptions label Filled)]
        | (ViewPoint p, Drawn label) -> [DrawPoint(applyPointTransform trans p, poptions label Hollow)]
        | (ViewSegment chain, Preview label) -> 
            chain
            |> List.map (fun s -> DrawSegment(applySegmentTransform trans s, soptions label Dotted))
        | (ViewSegment chain, Drawn label) -> 
            chain
            |> List.map (fun s -> DrawSegment(applySegmentTransform trans s, soptions label Solid))
        | (ViewLine line, Preview label) -> [DrawSegment(applyLineTransform trans line, soptions label Dotted)]
        | (ViewLine line, Drawn label) -> [DrawSegment(applyLineTransform trans line, soptions label Solid)]

    let drawableFromEvalResult (evalResult: E.EvalResult) (solve: S.Solver) targets = 
        let (views, viewErrs) = viewsFromEvalResult evalResult solve 
        let absolutePoints = extractAbsolutes evalResult
        let trans = pointTranslation targets absolutePoints
        // printf "%A" trans
        let drawables = List.collect (toDrawable trans) views
        (drawables, viewErrs)

