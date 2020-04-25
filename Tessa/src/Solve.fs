namespace Tessa.Solve

open System
open FSharp.Collections
open Tessa.Language 
open Tessa.Util
open System.Collections.Generic

// todo: Split into more submodules. Solve.Types, Solve.Line (for line solver) etc.?
module Solve =
    module L = Language
    open Util
    module S = State

    type Point = {x: double; y: double;}
    // https://stackoverflow.com/a/26565842/10558918
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Point = 
        let x: Point -> double = fun p -> p.x
        let y: Point -> double = fun p -> p.y

    let equalEnough p q = 
        let eps = 0.001
        (abs (p.x - q.x)) < eps && (abs (p.y - q.y)) < eps

    type Segment = 
        | Straight of Point * Point

    type SegmentChain = Segment list

    type Line = 
        | Vertical of x: double
        | Sloped of xy: Point * m: double

    type Location = double

    type PolygonId = PolygonId of int

    type PolygonDirectionality = Up | Down

    type Polygon = 
        | Polygon of PolygonId * Point * Segment * PolygonDirectionality

    // todo: look for calls to okay
    type SolveError = 
        | PointOnEmptySegmentChain of Location * SolveError option
        | LinePerpendicularToSegmentChain of string * SolveError option
        | ExtendSegmentToLine of string * SegmentChain * SolveError option
        | LineVerticalThroughX of Location * SolveError option
        | LineHorizontalThroughY of Location * SolveError option
        | PointLineIntersect of string * Line * Line * SolveError option
        | SegmentPerpendicular of string * SegmentChain * SegmentChain * SolveError option
        | SegmentSnipped of Segment * SolveError option
        | MergeSegmentChains of string * SegmentChain * SegmentChain * SolveError option

    let distance p q =
        sqrt <| (p.x - q.x)**2.0 + (p.y - q.y)** 2.0

    let length segment = 
        // https://gist.github.com/tunght13488/6744e77c242cc7a94859
        // let quadraticBezierLength (orig: Point) (control: Point) (dest: Point) =
        //    let ax = orig.x - 2.0 * control.x + dest.x
        //    let ay = orig.y - 2.0 * control.y + dest.y
        //    let bx = 2.0 * control.x - 2.0 * orig.x
        //    let by = 2.0 * control.y - 2.0 * orig.y
        //    let A = 4.0 * (ax * ax + ay * ay)
        //    let B = 4.0 * (ax * bx + ay * by)
        //    let C = bx * bx + by * by
        //    let Sabc = 2.0 * sqrt(A+B+C)
        //    let A_2 = sqrt(A)
        //    let A_32 = 2.0 * A * A_2
        //    let C_2 = 2.0 * sqrt(C)
        //    let BA = B / A_2

        //    (A_32 * Sabc + A_2 * B * (Sabc - C_2) + (4.0 * C * A - B * B) * log((2.0 * A_2 + BA + Sabc) / (BA + C_2))) / (4.0 * A_32);

        let straightLength orig dest =
            let square x = x * x
            (square (orig.x - dest.x)) + (square (orig.y - dest.y)) |> sqrt

        match segment with
            | Straight (orig, dest) -> straightLength orig dest

    let slope orig dest = 
        match dest.x - orig.x with
            | 0.0 -> None
            | _ -> Some <| (dest.y - orig.y) / (dest.x - orig.x)

    let evaluateLineAt orig dest x =
         slope orig dest 
        |> Option.map (fun m -> m * (x - dest.x) + dest.y)

    let evaluateLineAtWithSlope m point x = 
        m * (x - point.x) + point.y

    let pointOnStraightSegment orig dest location = 
        let dx = dest.x - orig.x
        let dy = dest.y - orig.y
        let newX = orig.x + dx*location
        match evaluateLineAt orig dest newX with
            | None -> {x = orig.x; y = orig.y + dy*location}
            | Some y -> {x = newX; y = y}

    let pointOnSegmentChain (segmentChain: SegmentChain) (location: Location) : Result<Segment * Point, SolveError> = 
        let accumulate (cumulative, m) head =
            let lengthHere = length head
            let newCumulative = lengthHere + cumulative
            (newCumulative, Map.add head (cumulative, newCumulative) m)

        let (_, cumulatives) = List.fold accumulate (0.0, Map.empty) segmentChain
        let total = List.sumBy length segmentChain
        let hitAt = location * total

        let getOnSegment segment orig dest = 
            let (start, finish) = Map.find segment cumulatives
            let locationOnThisSegment = (hitAt - start) / (finish - start)
            (segment, pointOnStraightSegment orig dest locationOnThisSegment)

        let choiceSegment = segmentChain |> List.skipWhile (fun s -> Map.find s cumulatives |> snd |> (>) hitAt) |> List.tryHead

        match choiceSegment with
            | None -> 
                match List.tryLast segmentChain with
                | None -> Error <| PointOnEmptySegmentChain (location, None)
                | Some(Straight(orig, dest) as segment) -> Ok <| getOnSegment segment orig dest
            | Some(Straight(orig, dest) as segment) -> Ok <| getOnSegment segment orig dest

    let solveLinePerpendicular (location: Location) (segmentChain: SegmentChain) = 
        Result.result {
            let! segmentPoint = pointOnSegmentChain segmentChain location
            return 
                match segmentPoint with
                | (Straight(orig, dest), point) -> 
                        match slope orig dest with
                        // Vertical lines become horizontal lines with slope=0
                        | None -> Sloped(point, 0.0)
                        // Horizontal lines become vertical lines
                        | Some 0.0 -> point |> Point.x |> Vertical
                        // All other lines have m -> -1/m
                        | Some m -> Sloped(point, -1.0 / m)
        } |> Result.mapError (fun e -> LinePerpendicularToSegmentChain ("Can't find line perpendicular to empty segment", (Some e)))

    let solveLineExtendSegment segmentChain = 
        match List.length segmentChain with
        | 0 -> Error <| ExtendSegmentToLine("No segments in chain; unable to extend to line", segmentChain, None)
        | 1 ->
            match List.head segmentChain with 
            | Straight(orig, dest) -> 
                match slope orig dest with
                | None -> Ok <| Vertical orig.x
                | Some m -> Ok <| Sloped (orig, m)
        | _ -> Error <| ExtendSegmentToLine("more than 1 segment in chain; unable to extend to line", segmentChain, None)

    let solveLineVerticalThroughX location segmentChain =
        Result.bimap 
            (snd >> Point.x >> Vertical) 
            (fun e -> LineVerticalThroughX (location, Some e))
            (pointOnSegmentChain segmentChain location)

    let solveLineHorizontalThroughY location segmentChain =
        Result.bimap 
            (snd >> fun p -> Sloped(p, 0.0)) 
            (fun e -> LineHorizontalThroughY (location, Some e))
            (pointOnSegmentChain segmentChain location)

    let rotateAround point around direction degree = 
        let degreeAsInt = 
            match degree with
            | L.C2 -> 180
            | L.C3 -> 120
            | L.C4 -> 90
            | L.C6 -> 60

        let degreeAsClockwise = 
            match direction with
            | L.Clockwise -> 360 - degreeAsInt
            | L.CounterClockwise -> degreeAsInt

        let radians = (float degreeAsClockwise) * Math.PI/180.0 

        {x = Math.Cos(radians) * (point.x - around.x) - Math.Sin(radians) * (point.y - around.y) + around.x;
        y = Math.Sin(radians) * (point.x - around.x) + Math.Cos(radians) * (point.y - around.y) + around.y;}
    
    // let solvePointOperated point operation =
    //     match operation with
    //     | L.Rotate(direction, angle, center) -> rotateAround point center direction angle

    let solvePointLineIntersect m n = 
        match (m, n) with
        | (Vertical(_), Vertical(_)) -> Error <| PointLineIntersect("There is no single point at which two vertical lines intersect.", m, n, None)
        | (Sloped(point, slope), Vertical(x)) -> Ok {x = x; y = evaluateLineAtWithSlope slope point x}
        | (Vertical(x), Sloped(point, slope)) -> Ok {x = x; y = evaluateLineAtWithSlope slope point x}
        | (Sloped(point1, slope1), Sloped(point2, slope2)) -> 
            if slope1 = slope2
            then Error <| PointLineIntersect("The lines are either parallel or identical -- no single point of intersection", m, n, None)
            else 
                let x = (slope1 * point1.x - slope2 * point2.x + point2.y - point1.y) / (slope1 - slope2)
                let y = evaluateLineAtWithSlope slope1 point1 x
                Ok {x = x; y = y;}

    let solveSegmentSnipped (original: SegmentChain) (cutAt: SegmentChain) = 
        let pointBoundedBy point p q =
            (min p.x q.x) <= point.x && point.x <= (max p.x q.x) && (min p.y q.y) <= point.y && point.y <= (max p.y q.y)

        let pointWithinSegmentBounds point segment = 
            match segment with
            | Straight(orig, dest) -> pointBoundedBy point orig dest

        let segmentsIntersect s1 s2 =  Result.fromOk None <| Result.result {
            let! extend1 = solveLineExtendSegment [s1]
            let! extend2 = solveLineExtendSegment [s2] 
            let! intersect = solvePointLineIntersect extend1 extend2
            return 
                if (pointWithinSegmentBounds intersect s1) && (pointWithinSegmentBounds intersect s2) 
                then Some intersect 
                else None
        } 

        let replaceEnd segment point =
            match segment with
                | Straight(orig, dest) -> Straight(orig, point)

        let rec search segments = 
            match segments with
            | [] -> []
            | segment::rest -> 
                let possibleCut = List.map (segmentsIntersect segment) cutAt |> List.filter Option.isSome |> List.tryHead
                match possibleCut with
                    | Some (Some cutPoint) -> [replaceEnd segment cutPoint]
                    | _ -> segment :: (search rest)

        search original

    let solveSegmentPerpendicular position (origSegment: SegmentChain) (endSegment: SegmentChain) =
        Result.result {
            let! (_, startPoint) = pointOnSegmentChain origSegment position
            let! perpLine = solveLinePerpendicular position origSegment
            let (>>=) x f = Result.bind f x
            let intersectionPoints = 
                endSegment 
                |> List.map (fun x -> Ok [x] >>= solveLineExtendSegment >>= (solvePointLineIntersect perpLine))
                |> okays
            return! 
                match intersectionPoints with
                | [] -> Error <| SegmentPerpendicular("Found no intersection between segments", origSegment, endSegment, None)
                | _ -> Ok <| Straight(startPoint, List.minBy (distance startPoint) intersectionPoints)
        }

    let origDest = function
        | Straight(orig, dest) -> (orig, dest)

    let mergeSegmentChains chain1 chain2 =
        let (last1, first2) = (List.last chain1, List.head chain2)
        // This is for when we add arcs

        let (_, last1dest) = origDest last1
        let (first2orig, first2dest) = origDest first2

        if equalEnough last1dest first2orig
        // Rebuild first point with last1dest to account for any minute differences
        then Ok <| chain1 @ (Straight(last1dest, first2dest) :: List.tail chain2)
        else Error <| MergeSegmentChains("The last point of chain1 is not close enough to the first point of chain2", chain1, chain2, None)

    let extendSegmentToPoint chain point =
        let (_, lastPointInChain) = origDest <| List.last chain
        chain @ [Straight(lastPointInChain, point)]

    type SolveContext = {PointContext: Map<L.Point, Point>; SegmentContext: Map<L.Segment, SegmentChain>; LineContext: Map<L.Line, Line>}

    // let rec solveSegment solvePoint solveLine tryFindSegment returnSegment segment = 
    //     let go = solveSegment solvePoint solveLine tryFindSegment returnSegment
    //         // assert last point in 
    //     match tryFindSegment segment with
    //         | Some chain -> chain
    //         | None -> returnSegment segment <| match segment with
    //             | L.Link(p1, p2) -> Straight(solvePoint p1, solvePoint p2)
    //             | L.Chain(s, p) 


    let solve (segments: L.Segment list) = 
        let mutable context = {PointContext = Map.empty; SegmentContext = Map.empty; LineContext = Map.empty;}

        let returnPoint orig solved =
            context <- {context with PointContext = Map.add orig solved context.PointContext}
            solved

        let returnSegment orig solved =
            context <- {context with SegmentContext = Map.add orig solved context.SegmentContext}
            solved

        let returnLine orig solved =
            context <- {context with LineContext = Map.add orig solved context.LineContext}
            solved

        let rec solvePoint (lpoint: L.Point) =
            match Map.tryFind lpoint context.PointContext with
                | Some p -> p
                | None -> 
                    returnPoint lpoint <| 
                    match lpoint with
                    | L.Absolute(x, y) as absolute -> {x = x; y = y}
                    | L.Operated(origin, op) -> 
                        match op with
                        | L.Rotate(direction, angle, center) -> rotateAround (solvePoint origin) (solvePoint center) direction angle
                        | L.GlideAround(_) -> failwith "get to this later"
                    | L.OnSegment(L.PointOnSegment(position, segment)) -> pointOnSegmentChain (solveSegment segment) position |> snd 
                    // todo: should I be using Result based error handling in here? I don't see why I should plumb inside when I could catch on the outside
                    // and maintain a pure interface
                    | L.Intersection(line1, line2) -> solvePointLineIntersect (solveLine line1) (solveLine line2) |> okay

        and solveSegment (segment: L.Segment) : SegmentChain = 
            match Map.tryFind segment context.SegmentContext with
                | Some chain -> chain
                | None -> 
                    returnSegment segment <| 
                    match segment with
                    | L.Link(p1, p2) -> [Straight(solvePoint p1, solvePoint p2)]
                    | L.Chain(s, p) -> extendSegmentToPoint (solveSegment s) (solvePoint p)
                    | L.Concat(s1, s2) -> mergeSegmentChains (solveSegment s1) (solveSegment s2) |> okay
                    | L.Perpendicular(position, originSegment, endSegment) -> [solveSegmentPerpendicular position (solveSegment originSegment) (solveSegment endSegment)]
                    | L.Snipped(orig, cutAt) -> solveSegmentSnipped (solveSegment orig) (solveSegment cutAt)

        and solveLine (line: L.Line) : Line = 
            match Map.tryFind line context.LineContext with 
                | Some l -> l 
                | None ->
                    returnLine line <|
                    match line with
                    | L.Line.Perpendicular(pos, segment) -> solveLinePerpendicular pos (solveSegment segment)
                    | L.VerticalThroughX(x, segment) -> solveLineVerticalThroughX x (solveSegment segment)
                    | L.HorizontalThroughY(y, segment) -> solveLineHorizontalThroughY y (solveSegment segment)
                    | L.ExtendSegment(segment) -> solveLineExtendSegment (solveSegment segment) |> okay

        List.map solveSegment segments


    // let solve segments = 
    //     // todo: I'm not actually using the cache anywhere
    //     let context = makeContext ()

    //     let rec solvePoint (lpoint: L.Point) =
    //         let ret p = 
    //             context.PointContext.Add(lpoint, p)
    //             p

    //         match lpoint with
    //             | L.Absolute(x, y) -> ret <| {x = x; y = y;}

    //             | L.OnSegment(L.PointOnSegment(position, segment)) -> 
    //                 let {orig = orig; dest = dest;} = solveSegment segment
    //                 ret <| pointOnSegment orig dest position

    //             | L.Operated(origin, operation) -> ret <| {x = 0.0; y= 0.0;}
    //             | L.Intersection(line1, line2) -> ret <| {x = 0.0; y = 0.0;}

    //     and solveSegment (lsegment: L.Segment) = 
    //         let ret s = 
    //             context.SegmentContext.Add(lsegment, s)
    //             s
            
    //         match lsegment with
    //             | L.Singlet(point) -> 
    //                 let solvedPoint = solvePoint point
    //                 ret {orig = solvedPoint; dest = solvedPoint;}
    //             | L.Concat(s1, s2) ->
    //                 let solved1, solved2 = solveSegment s1, solveSegment s2

    //             // | Perpendicular of  position: double * originSegment: Segment * endSegment: Segment
    //             // | Concat of Segment * Segment
    //             // | QuadraticBezier of ``from``: Point * ``to``: Point * control: Point
    //             // | Snipped of original: Segment * cutAt: Segment

    //     []





