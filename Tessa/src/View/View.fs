namespace Tessa.View

open Tessa.View.Types
open Tessa.Eval
open Tessa.Eval.Types
open Tessa.Solve.Shapes
open Tessa.Solve.Types
open Tessa.Solve.Polygons
open Tessa.Language
open Tessa.Util

module View = 
    open ViewTypes
    module E = EvalTypes
    module S = SolveShapes
    module S = SolveTypes
    module S = SolvePolygons
    module L = Language
    open Util

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

    let pointTranslation targets (allPoints: S.Point list) = 
        let getX (p: S.Point) = p.x
        let getY (p: S.Point) = p.y
        let bounds points = 
            let minX = List.minBy getX points |> getX
            let maxX = List.maxBy getX points |> getX
            let minY = List.minBy getY points |> getY
            let maxY = List.maxBy getY points |> getY
            (minX, maxX, minY, maxY)

        // todo: if just 0 or 1 points or all colinear ugh
        // we need three non-colinear points for a plane.
        let (minX, maxX, minY, maxY) = bounds allPoints
        let scaleFactor = min (float targets.boundingWidth / (maxX - minX)) (float targets.boundingHeight / (maxY - minY))
        let scale (p: S.Point) = {S.x = scaleFactor * p.x;  S.y = scaleFactor * p.y}

        let newAbsolutes = List.map scale allPoints
        let (newMinX, newMaxX, newMinY, newMaxY) = bounds newAbsolutes
        let xTrans = (fst targets.topLeft) - newMinX
        let yTrans = (snd targets.topLeft) - newMinY

        {xScale = scaleFactor; yScale = scaleFactor; xTrans = xTrans; yTrans = yTrans;
        xMin = newMinX; xMax = newMaxX; yMin = newMinY; yMax = newMaxY;
        absoluteXMax = targets.xMax; absoluteYMax = targets.yMax;}

    let applyPointTransform trans (point: S.Point) = 
        {S.x = trans.xScale * point.x + trans.xTrans; S.y = trans.yScale * point.y + trans.yTrans;}
    
    let rec indexAppliesTo lindex pindex = 
        match (lindex, pindex) with
        | ([], []) -> true
        | (L.Ind n :: ls, m :: ps) -> n = m && indexAppliesTo ls ps
        | _ -> false

    let solveTessellation targets (L.Tessellation(cell, effects)) (labeledPoints: Map<string, L.Point>) =
        result {
            let! rawPolygons = S.solveCell cell
            let! solvedLabeledPoints = Map.mapList (fun label point -> S.solve.point point |> Result.map (fun p -> (label, p))) labeledPoints |> Result.sequence
            let transform = pointTranslation targets <| (List.map (fun (x, y) -> y) solvedLabeledPoints) @ S.allPoints rawPolygons
            let scaledPolygons = S.mapPointsPolygons (applyPointTransform transform) rawPolygons
            let scaledLabeledPoints = List.map (fun (l, p) -> (l, applyPointTransform transform p)) solvedLabeledPoints
            // let scaledLabelPoints = 

            let toTup (p: S.Point) = (p.x, p.y)
            let toDrawPoly (polygon: S.Polygon) = 
                let origDests = 
                    match polygon.segments with 
                    | (S.Straight(p, q) :: restSegments) ->
                        toTup p 
                        :: toTup q 
                        :: (restSegments |> List.map (fun (S.Straight(_, q)) ->  toTup q))
                    | _ -> []
                        // |> List.distinct
                DrawPolygon(origDests, {color = "#004080"})
            
            let toDrawPointFromPoly i (p: S.Polygon) =
                DrawPoint ((p.centroid.x, p.centroid.y), {color = "#004080"; label = String.concat "." (List.map string p.index)})
            
            return List.map toDrawPoly scaledPolygons
                @ List.mapi toDrawPointFromPoly scaledPolygons
                @ List.map (fun (label, point: S.Point) -> DrawPoint((point.x, point.y), {color = "#004080"; label=label})) scaledLabeledPoints
        }

    // let viewsFromTessellation ((L.Tessellation(cell, effects)): L.Tessellation) (pointLabels: Map<string, L.Point>) =
    // result {
    //         // let! solvedPoints = pointLabels |> Map.mapList (fun k v -> S.solve.point v) |> Result.sequence
    //         // todo: provision pointIds with canonState, return if showPolygonCentroid is true
    //         return (List.map ViewPolygon polygons, canonState)
    //     }


    // let viewsFromEvalResult (evalResult: E.EvalResult)  
    //     : View list * (ViewMode * S.SolveError) list =
    //     let rec resultPartition = function 
    //         | [] -> ([], [])
    //         | (viewMode, Ok solved) :: rest -> 
    //             let (oks, errs) = resultPartition rest 
    //             ((viewMode, solved) :: oks, errs)
    //         | (viewMode, Error err) :: rest ->
    //             let (oks, errs) = resultPartition rest
    //             (oks, (viewMode, err) :: errs)
        
    //     let toView (mode, shape) = {viewShape = shape; viewMode = mode;}

    //     // todo: long term, solving shouldn't happen in here. There will be an extra layer that computes polygons,
    //     // applies tessellations, determines indexes. 

    //     let (env, draw, result) = (evalResult.runtime.environment, evalResult.runtime.drawMap, evalResult.value)
    //     // we should be able to handle partial errors
    //     let (fromEnvOks, fromEnvErrors) = 
    //         env 
    //         |> Map.filterSome isGeoExp 
    //         |> Map.mapList (fun label shape -> (Preview label, solveGeoExp shape)) 
    //         |> resultPartition

    //     let (fromDrawOks, fromDrawErrors) = 
    //         draw 
    //         |> Map.mapListMany (fun (E.CellName category) shapes -> List.map (fun shape -> (Drawn category, solveGeoExp shape)) shapes) 
    //         |> resultPartition

    //     let fromResult = result |> Option.bind isGeoExp |> Option.map (fun s -> [(Preview "Current Exp", solveGeoExp s)])

    //     let (retOkays, retErrs) = (List.map toView <| fromEnvOks @ fromDrawOks, fromEnvErrors @ fromDrawErrors)

    //     // todo: make sure intersections between draw and result/env are shown only once
    //     match Option.map resultPartition fromResult with 
    //         | Some(okays, errs) -> (List.map toView okays @ retOkays, errs @ retErrs)
    //         | None -> (retOkays, retErrs)

    // let applySegmentTransform trans (seg: S.Segment) = 
    //     match seg with 
    //     | S.Straight(orig, dest) -> {orig = applyPointTransform trans orig; dest = applyPointTransform trans dest}
    
    // let applyLineTransform trans (line: S.Line) =   
    //     match line with 
    //     | S.Vertical(x) ->
    //         let newX = trans.xScale * x + trans.xTrans 
    //         {orig = {x = newX; y = 0.0;}; dest = {x = newX; y = trans.absoluteYMax;}}
    //     | S.Sloped(point, m) ->
    //         let newPoint = applyPointTransform trans point 
    //         let y x =  m * (x - newPoint.x) + newPoint.y
    //         // todo: need to solve for x that has max y
    //         {orig = {x = trans.xMin; y = y 0.0;}; dest = {x = trans.xMax; y = y trans.yMax;}}

    // todo: Somehow need a bounding box 
    // let toDrawable trans view = 
    //     let poptions label style = {color = "#004080";}
    //     let soptions label style = {color = "#004080";}
    //     match (view.viewShape, view.viewMode) with 
    //     | (ViewPoint p, Preview label) -> [DrawPoint(applyPointTransform trans p, poptions label Filled)]
    //     | (ViewPoint p, Drawn label) -> [DrawPoint(applyPointTransform trans p, poptions label Hollow)]
    //     | (ViewSegment chain, Preview label) -> 
    //         chain
    //         |> List.map (fun s -> DrawSegment(applySegmentTransform trans s, soptions label Dotted))
    //     | (ViewSegment chain, Drawn label) -> 
    //         chain
    //         |> List.map (fun s -> DrawSegment(applySegmentTransform trans s, soptions label Solid))
    //     | (ViewLine line, Preview label) -> [DrawSegment(applyLineTransform trans line, soptions label Dotted)]
    //     | (ViewLine line, Drawn label) -> [DrawSegment(applyLineTransform trans line, soptions label Solid)]

    // let drawableFromEvalResult (evalResult: E.EvalResult) targets = 
    //     let (views, viewErrs) = viewsFromEvalResult evalResult
    //     let absolutePoints = extractAbsolutes evalResult
    //     let trans = pointTranslation targets absolutePoints
    //     // printf "%A" trans
    //     let drawables = List.collect (toDrawable trans) views
    //     (drawables, viewErrs)

