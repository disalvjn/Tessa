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
        let r (x: float) : float = Math.Round (x, 5) 
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
        // failAndPrint segments
        let result = List.collect (fun s -> atomizeSegment s (List.filter ((<>) s) segments)) segments |> List.map roundSegment
        result

    type SegmentSet = Set<Set<Point>>

    let polygonIsSuperset (s1: Segment list) (s2: Segment list) = 
        // it's point based, not segment based
        let allPoints = List.collect (fun (Straight(p, q)) -> [p; q;]) >> Set.ofList
        let allPointsP1 = allPoints s1 
        let allPointsP2 = allPoints s2 

        let p1Lines = List.map (fun (Straight(p, q)) -> S.segmentToLine <| Straight(p, q)) s1

        let (&&&) f g = fun x y -> f x y && g x y

        let pointFromP2InsideP1 p2 = 
            let above p q = p.y > q.y
            let below p q = p.y < q.y
            let right p q = p.x > q.x
            let left p q = p.x < q.x
            let againstLine line f g = 
                let intersections = List.map (S.solvePointLineIntersect line) p1Lines |> okays |> List.distinct
                List.length (List.filter (f p2) intersections) % 2 = 1
                && List.length (List.filter (g p2) intersections) % 2 = 1
            againstLine (Vertical p2.x) above below 
            && againstLine (Sloped(p2, 0.0)) right left

        let pointsToCheck = (Set.toList <| Set.difference allPointsP2 allPointsP1)

        if List.isEmpty pointsToCheck 
        then false 
        else List.exists pointFromP2InsideP1  pointsToCheck

    type PolyBfsState = {
        target: Point;
        steps: Point list;
        walkedEdges: Set<Point * Point>;
    }


    let joinToPolygonsAsSegments (segments : Segment list) : Segment list list = 
        let hasWalked polyBfsState point1 point2 = 
            Set.contains (point1, point2) polyBfsState.walkedEdges || Set.contains (point2, point1) polyBfsState.walkedEdges

        let graphStep =  
            segments
            |> List.collect (fun (Straight(p, q)) -> [(p, q); (q, p);])
            |> List.groupBy fst
            |> List.map (fun (orig, dests) -> (orig, List.map snd dests |> List.distinct))
            |> Map.ofList

        let nextSteps polyBfs = 
            let currentPoint = (List.head polyBfs.steps)
            let nextPoints = Map.find currentPoint graphStep |> List.filter (complement (hasWalked polyBfs currentPoint))
            List.map 
                (fun nextPoint -> 
                    {polyBfs with 
                        steps = nextPoint :: polyBfs.steps; 
                        walkedEdges = Set.add (currentPoint, nextPoint) polyBfs.walkedEdges}) 
                nextPoints

        let polygonsContaining (Straight(p, q): Segment) =
            // intentionally no visited
            let rec go (queue: PolyBfsState list) : PolyBfsState list = 
                let nextQueue = List.collect nextSteps queue
                let closedPolygons = List.filter (fun pbfs -> pbfs.target = List.head pbfs.steps) nextQueue
                if not <| List.isEmpty closedPolygons 
                then closedPolygons
                else if List.isEmpty nextQueue
                then []
                else go nextQueue

            go [{target = q; walkedEdges = Set.add (p, q) Set.empty; steps = [p; q]}]

        let polyBfsToSegmentList polyBfs = 
            Seq.zip polyBfs.steps (List.tail polyBfs.steps) |> Seq.toList |> List.map (fun (p, q) -> Straight(p, q))

        let allPointsIn segmentList = 
            List.collect (fun (Straight(p, q)) -> [p; q;]) segmentList // |> Set.ofList

        let x = 
            List.collect polygonsContaining segments // (allPointsIn segments |> List.distinct) 
            |> List.map polyBfsToSegmentList
            |> List.distinctBy (allPointsIn >> Set.ofList)

        List.filter (fun p ->  not <| List.exists (fun p2 -> polygonIsSuperset p p2) x) x

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
