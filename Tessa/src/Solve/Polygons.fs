namespace Tessa.Solve.Polygons

open System
open FSharp.Collections
open Tessa.Language 
open Tessa.Util
open System.Collections.Generic
open Tessa.Solve.Types
open Tessa.Solve.Shapes

// todo: Split into more submodules. Solve.Types, Solve.Line (for line solver) etc.?
module SolvePolygons =
    open SolveTypes 
    open Util
    module S = SolveShapesInternal
    module S = SolveShapes
    module L = Language

    let roundSegment (Straight(p, q)) = 
        let r (x: float) : float = x // Math.Round (x, 3) 
        Straight({x = r p.x; y =  r p.y;}, {x = r q.x; y = r q.y;})

    let atomizeSegment segment chain = 
        let rec splits atoms = 
            match atoms with 
            | [] -> Set.empty
            | atom :: xs -> 
                let nextSplits = List.map (fun s -> S.solveSegmentSnipped atom [s]) chain |> somes |> List.unpack |> Set.ofList
                if not (Set.isEmpty nextSplits) then Set.union nextSplits (splits xs) else Set.add atom (splits xs)

        let rec go atoms = 
            let afterSplit = splits (List.ofSeq atoms)
            if Set.count afterSplit = Set.count atoms 
            then afterSplit
            else go afterSplit

        Set.ofList [segment] |> go |> Set.toList 

    let atomizeSegments segments =
        let rounded = List.map roundSegment segments
        // failAndPrint rounded
        let result = List.collect (fun s -> atomizeSegment s segments) rounded |> List.map roundSegment
        result

    let closed (pointSet: Set<Set<Point>>) = 
        let rec asTuples pointLinks visitedBegin = 
            match pointLinks with
            | [] -> []
            | [p; q] :: rest -> 
                if Set.contains p visitedBegin
                then (q, p) :: (asTuples rest <| Set.add q visitedBegin)
                else (p, q) :: (asTuples rest <| Set.add p visitedBegin)
            | _ -> failwith "should be impossible"

        let orderThem tuples = 
            let begToTuple = List.map (fun (x, y) -> (x, (x, y))) tuples |> Map.ofList
            match tuples with
            | t :: ts -> 
                let unfoldFn (visited, tup) =
                    if Set.contains tup visited
                    then None 
                    else 
                        let (thisO, thisD) = tup
                        Some (tup, (Set.add tup visited, Map.find thisD begToTuple))
                List.unfold unfoldFn (Set.empty, t)
            | [] -> []

        let tupled = asTuples (Set.toList (Set.map Set.toList pointSet)) Set.empty
        let (beginnings, endings) = List.unzip tupled
        let closedPath = Set.ofList beginnings = Set.ofList endings
        if closedPath then Some <| List.map Straight (orderThem tupled) else None

    // todo: this is not true
    let polygonIsSuperset2 (p1: Set<Set<Point>>) (p2: Set<Set<Point>>) = 
        // it's point based, not segment based
        let allPointsP1 = p1 |> Set.toList |> List.collect Set.toList |> Set.ofList
        let allPointsP2 = p2 |> Set.toList |> List.collect Set.toList |> Set.ofList

        let p1Lines = 
            Set.toList p1 
            |> List.map (fun set ->
                match Set.toList set with 
                | [p; q;] -> S.segmentToLine <| Straight(p, q)
                | _ -> failwith "impossible")

        // let withLineStartingFromP1PointP2LiesInside p1Point p2Point = 
        //     let thisLine = S.segmentToLine <| Straight(p1Point, p2Point)
        //     let intersections = 
        //         p1Lines
        //         |> List.map (fun line -> S.solvePointLineIntersect line thisLine) 
        //         |> okays
        //         |> List.map (S.distance p1Point)
        //         |> List.filter (fun x -> x > 0.00001)
        //     if List.isEmpty intersections
        //     then false 
        //     else
        //         let closest = List.min intersections
        //         // This could cause a bug -- what if the points are in opposite directions?
        //         closest > S.distance p1Point p2Point 

        let pointFromP2InsideP1 p2 = 
            let above p q = p.y > q.y
            let below p q = p.y < q.y
            let right p q = p.x > q.x
            let left p q = p.x < q.x
            let againstLine line f g = 
                let intersections = List.map (S.solvePointLineIntersect line) p1Lines |> okays 
                List.length (List.filter (f p2) intersections) % 2 = 1
                && List.length (List.filter (g p2) intersections) % 2 = 1
            againstLine (Vertical p2.x) above below && againstLine (Sloped(p2, 0.0)) right left

        // let pointTuckedAwayCozilyInsideP1 p2 =
        //     // false
        //     if Set.contains p2 allPointsP1
        //     then false
            // (fun p1 -> withLineStartingFromP1PointP2LiesInside p1 p2) <| Set.toList (Set.difference allPointsP1 allPointsP2)

        let pointFromP2InsideP1 =  List.exists pointFromP2InsideP1 (Set.toList <| Set.difference allPointsP2 allPointsP1)
        let result = Set.isSuperset allPointsP1 allPointsP2 || pointFromP2InsideP1
        // List.exists pointTuckedAwayCozilyInsideP1 (Set.toList allPointsP2)
        result

    // join must work when only some segments form completed polygons and must allow other segments to continue existing
    let joinToPolygonsAsSegments (segments : Segment list) : Segment list list = 

        // It is 2020 after all...
        let rec go (points: Point list) (visitedPoints: Set<Point>) (candidates: Set<Set<Point>> list) (elected: Set<Set<Point>> list) = 
            match points with 
            | [] -> elected |> List.filter (fun e -> not <| List.exists (fun e2 -> e <> e2 && polygonIsSuperset2 e e2) elected) |> List.map closed |> somes 
            | p :: ps -> 
                let hasSegmentUsingPoint candidate = Set.exists (fun pointSet -> Set.contains p pointSet) candidate
                let (segmentsInCandidatesUsingPoint, unelectableCandidates) = List.partition hasSegmentUsingPoint candidates
                let augmented = 
                    List.cartesianProduct segmentsInCandidatesUsingPoint segmentsInCandidatesUsingPoint
                    |> List.collect (fun (x, y) -> [Set.union x y; x; y])
                    |> List.append unelectableCandidates
                    |> List.distinct

                let augmentedWithoutSupersets = List.filter (fun aug -> not <| List.exists (fun poly -> polygonIsSuperset2 aug poly) elected) augmented

                let (newPolygons, newCandidates) = List.partition (Option.isSome << closed) augmentedWithoutSupersets
                let prunedExistingPolygons = List.filter (fun poly -> not <| List.exists (fun newPoly -> polygonIsSuperset2 poly newPoly) newPolygons) elected

                go ps (Set.add p visitedPoints) newCandidates (newPolygons @ prunedExistingPolygons)

        let allPoints = List.collect (fun (Straight(p, q)) -> [p; q]) segments |> List.distinct
        let initialCandidates = List.map (fun (Straight(p, q)) -> Set.ofList [Set.ofList [p; q]]) segments

        go allPoints Set.empty initialCandidates []

    let orderByCentroids (polygons: Segment list list) = 
        let centroid points = {
            x = List.sumBy (fun p -> p.x) points / (float <| List.length points); 
            y = List.sumBy (fun p -> p.y) points / (float <| List.length points)}
        // The polygon is closed, so every orig point is also a dest point and vice versa, so we only need to take one of the points from each segment.
        let polyCentroid polygon = centroid <| List.map (fun (Straight(p, q)) -> p) polygon

        let polygonsWithCentroids = List.map (fun p -> (polyCentroid p, p)) polygons 
        let sorted = List.sortBy (fun (centroid, p) -> (centroid.y, centroid.x)) polygonsWithCentroids
        List.mapi (fun i (centroid, polygon) -> {centroid = centroid; index = [i]; segments = polygon}) sorted

    let solvePolygons segments = 
        result {
            let! atomized = List.map S.solve.segment segments |> Result.sequence |> Result.map (List.concat >> atomizeSegments)
            // failAndPrint atomized
            let joinedAsSegments = joinToPolygonsAsSegments atomized 
            let polygons = orderByCentroids joinedAsSegments
            // failAndPrint polygons
            return polygons
        }

    let mirrorPointOverLine l p = 
        match l with 
        | Vertical(x) -> {x = (x - p.x  + x); y = p.y}
        // https://stackoverflow.com/a/3307181/10558918
        | Sloped(point, m) -> 
            let c = point.y - m * point.x
            let d = (p.x + (p.y - c)*m)/ (1.0 + m*m)
            {x = 2.0*d - p.x ; y = 2.0*d*m - p.y + 2.0*c}


    // Polygon * CanonState should be its own record with fns defined on it like mapPoint and mapPointId.
    // MapPoint automatically copies.
    let rec solveCell (cell: L.Cell) = 
        match cell with 
        | L.Primary segments -> solvePolygons segments
        | L.Transformed(op, cell) -> 
            result {
                let! solved = solveCell cell 
                return! 
                    match op with 
                    | L.MirrorOver(unsolvedLine)->
                        result {
                            let! line = S.solve.line unsolvedLine
                            let mappedOver = mapPointsPolygons (mirrorPointOverLine line) solved
                            return indexAppend [0] solved @ indexAppend [1] mappedOver
                        }
                    // Todo: Want different ones for C3, C4, C6
                    // This visit strategy works for C6 (inward -> outward) but not for c4
                    | L.Repeat(span, rotation, times) ->
                        result {
                            let! (Straight(orig, dest)) = S.solve.segment span |> Result.map List.head // todo: make that safer
                            let dist = S.distance orig dest
                            let midpoint = S.pointOnStraightSegment orig dest 0.5
                            let movePoint mag theta p = {x = p.x + cos(theta) * mag; y = p.y + sin(theta) * mag;} 
                            let thetas = 
                                (match rotation with
                                | L.C2 -> [0; 180;]
                                | L.C3 -> [0; 120; 240;]
                                | L.C4 -> [0; 90; 180; 270;]
                                | L.C6 -> [0; 60; 120; 180; 240; 300;])
                                |> List.map (fun x -> (float x) * Math.PI / 180.0) 

                            let rec allMidpoints times midPoints =
                                if times = 0
                                then midPoints
                                else 
                                    let genNext mps = Set.unionMany [for mp in mps do for theta in thetas -> Set.ofList [mp; movePoint dist theta mp;]]
                                    allMidpoints (times - 1) (midPoints |> genNext |> genNext)

                            let midPoints = 
                                allMidpoints times (Set.ofList [midpoint])
                                |> Set.toList
                                |> List.filter ((<>) midpoint) 
                                // this is hack trying to get C6 method to work with C4
                                |> List.filter (fun p -> S.distance p midpoint < dist * (float times + 1.0))

                            let movePolygonsTo nextMidpoint = 
                                let theta = Math.Atan2(nextMidpoint.y - midpoint.y, nextMidpoint.x - midpoint.x)
                                mapPointsPolygons (movePoint (S.distance nextMidpoint midpoint) theta) solved

                            return solved @ List.collect movePolygonsTo midPoints
                        }
            }
        | _ -> failwith "not here yet dawg"
