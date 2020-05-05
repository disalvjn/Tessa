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

    let equalEnoughEps p q eps = 
        (abs (p.x - q.x)) < eps && (abs (p.y - q.y)) < eps

    //
    // POLYGONS
    //

    let pointIdToPoint cstate pointId =
        // it's a bit sketkchy using find instead of tryFind. We'll get an exception
        // if there is no such id. But as long as we're getting ids by the state's provisioning we'll
        // be fine.
        Map.find pointId cstate.idToPoint 

    let segmentIdToSegment cstate (SegmentId(p, q)) = 
        Straight(pointIdToPoint cstate p, pointIdToPoint cstate q)

    let pointToPointId point = 
        state {
            // could optimize by having an incomplete pointToPointId map -- incomplete because
            // it wouldn't account for all possible epsilon-allowed variations. 
            // but we're not going from raw to id nearly as often as id to raw, so I don't think
            // it's worth the extra complexity for now.
            let! s = State.get
            let existing = Map.toList s.idToPoint |> List.filter (fun (id, q) -> equalEnoughEps point q s.epsilon) |> List.tryHead
            match existing with 
            | Some (pointId, _) -> return pointId
            | None -> 
                let newId = PointId s.nextId
                do! State.put {s with nextId = s.nextId + 1; idToPoint = Map.add newId point s.idToPoint}
                return newId
        }
    
    let segmentToSegmentId (Straight(orig, dest)) = 
        state {
            let! orig' = pointToPointId orig 
            let! dest' = pointToPointId dest 
            return SegmentId (orig', dest')
        }

    // let canonicalize points = 
    //     List.allPairs // map from original to canon
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
        let atomized = List.collect (fun s -> atomizeSegment s segments) segments 
        let canonicized = State.sequence <| List.map segmentToSegmentId atomized
        State.run canonicized emptyCanonicizerState

    let closed (pointSet: Set<Set<PointId>>) = 
        let rec asTuples pointLinks visitedBegin = 
            match pointLinks with
            | [] -> []
            | [p; q] :: rest -> 
                if Set.contains p visitedBegin
                then (q, p) :: (asTuples rest <| Set.add q visitedBegin)
                else (p, q) :: (asTuples rest <| Set.add p visitedBegin)
            | _ -> failwith "should be impossible"
        let tupled = asTuples (Set.toList (Set.map Set.toList pointSet)) Set.empty
        let (beginnings, endings) = List.unzip tupled
        let closedPath = Set.ofList beginnings = Set.ofList endings
        if closedPath then Some <| List.map SegmentId tupled else None

    // join must work when only some segments form completed polygons and must allow other segments to continue existing
    let joinToPolygons (segments : SegmentId list) : Polygon list = 

        let polygonIsSuperset (p1: Set<Set<PointId>>) (p2: Set<Set<PointId>>) = 
            // it's point based, not segment based
            let allPointsP1 = p1 |> Set.toList |> List.collect Set.toList |> Set.ofList
            let allPointsP2 = p2 |> Set.toList |> List.collect Set.toList |> Set.ofList
            Set.isSuperset allPointsP1 allPointsP2

        // It is 2020 after all...
        let rec go (points: PointId list) (visitedPoints: Set<PointId>) (candidates: Set<Set<PointId>> list) (elected: Set<Set<PointId>> list) = 
            match points with 
            | [] -> List.map closed elected |> somes |> List.map (fun segments -> {Polygon.segments = segments})
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

        let allPoints = List.collect (fun (SegmentId(p, q)) -> [p; q]) segments |> List.distinct
        let initialCandidates = List.map (fun (SegmentId(p, q)) -> Set.ofList [Set.ofList [p; q]]) segments

        go allPoints Set.empty initialCandidates []

    let orderByCentroids canonState polygons = 
        let pointIds polygon = List.collect (fun (SegmentId(p, q)) -> [p; q]) polygon |> List.distinct
        let centroid points = (List.sumBy (fun p -> p.x) points / (float <| List.length points), List.sumBy (fun p -> p.y) points / (float <| List.length points))
        let centroidOfIds pointIds = List.map (pointIdToPoint canonState) pointIds |> centroid
        let polyCentroid polygon = polygon.segments |> pointIds |> centroidOfIds

        let polygonsWithCentroids = List.map (fun p -> (polyCentroid p, p)) polygons 
        List.sortBy (fun (c, p) -> c) polygonsWithCentroids