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
        List.collect (fun s -> atomizeSegment s segments) segments 

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

    // join must work when only some segments form completed polygons and must allow other segments to continue existing
    let joinToPolygonsAsSegments (segments : Segment list) : Segment list list = 

        // todo: this is not true
        let polygonIsSuperset (p1: Set<Set<Point>>) (p2: Set<Set<Point>>) = 
            // it's point based, not segment based
            let allPointsP1 = p1 |> Set.toList |> List.collect Set.toList |> Set.ofList
            let allPointsP2 = p2 |> Set.toList |> List.collect Set.toList |> Set.ofList
            Set.isSuperset allPointsP1 allPointsP2

        // It is 2020 after all...
        let rec go (points: Point list) (visitedPoints: Set<Point>) (candidates: Set<Set<Point>> list) (elected: Set<Set<Point>> list) = 
            match points with 
            | [] -> List.map closed elected |> somes 
            | p :: ps -> 
                let hasSegmentUsingPoint candidate = Set.exists (fun pointSet -> Set.contains p pointSet) candidate
                let (segmentsInCandidatesUsingPoint, unelectableCandidates) = List.partition hasSegmentUsingPoint candidates
                let augmented = 
                    List.cartesianProduct segmentsInCandidatesUsingPoint segmentsInCandidatesUsingPoint
                    |> List.collect (fun (x, y) -> [Set.union x y; x; y])
                    |> List.append unelectableCandidates
                    |> List.distinct

                let augmentedWithoutSupersets = List.filter (fun aug -> not <| List.exists (fun poly -> polygonIsSuperset aug poly) elected) augmented

                let (newPolygons, newCandidates) = List.partition (Option.isSome << closed) augmentedWithoutSupersets
                let prunedExistingPolygons = List.filter (fun poly -> not <| List.exists (fun newPoly -> polygonIsSuperset poly newPoly) newPolygons) elected

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
            let joinedAsSegments = joinToPolygonsAsSegments atomized 
            let polygons = orderByCentroids joinedAsSegments
            return polygons
        }

    // Polygon * CanonState should be its own record with fns defined on it like mapPoint and mapPointId.
    // MapPoint automatically copies.
    let solveCell (cell: L.Cell) = 
        match cell with 
        | L.Primary segments -> solvePolygons segments
        | _ -> failwith "not here yet dawg"
