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
        | (L.Any :: ls, m :: ps) -> indexAppliesTo ls ps
        // | ([L.AllEndingAt i], x :: xs) -> 
        | _ -> false

    let solveTessellation targets (L.Tessellation(cell, effects)) (labeledPoints: Map<string, L.Point>) =
        result {
            let! rawPolygons = S.solveCell cell
            let! solvedLabeledPoints = Map.mapList (fun label point -> S.solve.point point |> Result.map (fun p -> (label, p))) labeledPoints |> Result.sequence
            let transform = pointTranslation targets <| (List.map (fun (x, y) -> y) solvedLabeledPoints) @ S.allPoints rawPolygons
            let scaledPolygons = S.mapPointsPolygons (applyPointTransform transform) rawPolygons
            let scaledLabeledPoints = List.map (fun (l, p) -> (l, applyPointTransform transform p)) solvedLabeledPoints

            let asColor = function 
                | L.Fill c -> Some c 
                | _ -> None

            let colorPolygon (polygon: S.Polygon) = 
                effects
                |> List.filter (fun (index, e) -> indexAppliesTo index polygon.index) 
                |> List.map (fun (_, e) -> 
                    match e with 
                    | L.Fill c -> Some {color = "#" + c; drawMode = Fill;}
                    | L.Stroke c -> Some {color = "#" + c; drawMode = Stroke;}
                    | _ -> None)
                |> somes
                |> List.tryHead
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
                DrawPolygon(origDests, Option.cata id {color = "#004080"; drawMode = Stroke;} (colorPolygon polygon))
            
            let toDrawPointFromPoly i (p: S.Polygon) =
                DrawPoint ((p.centroid.x, p.centroid.y), {color = "#004080"; label = String.concat "." (List.map string p.index)})
            
            return List.map toDrawPoly scaledPolygons
                @ List.mapi toDrawPointFromPoly scaledPolygons
                @ List.map (fun (label, point: S.Point) -> DrawPoint((point.x, point.y), {color = "#004080"; label=label})) scaledLabeledPoints
        }