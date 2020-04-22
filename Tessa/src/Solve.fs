namespace Tessa.Solve

open System
open FSharp.Collections
open Tessa.Language 
open System.Collections.Generic

module Solve =
    module L = Language

    type Point = {x: double; y: double;}
    // https://stackoverflow.com/a/26565842/10558918
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Point = 
        let x: Point -> double = fun p -> p.x
        let y: Point -> double = fun p -> p.y

    type Segment = 
        | Straight of Point * Point
        | QuadraticBezier of orig: Point * control: Point * dest: Point

    type SegmentChain = Segment list

    type Line = 
        | Vertical of x: double
        | Sloped of xy: Point * m: double

    type Location = double

    type PolygonId = PolygonId of int

    type PolygonDirectionality = Up | Down

    type Polygon = 
        | Polygon of PolygonId * Point * Segment * PolygonDirectionality

    // https://gist.github.com/tunght13488/6744e77c242cc7a94859
    let quadraticBezierLength (orig: Point) (control: Point) (dest: Point) =
       let ax = orig.x - 2.0 * control.x + dest.x
       let ay = orig.y - 2.0 * control.y + dest.y
       let bx = 2.0 * control.x - 2.0 * orig.x
       let by = 2.0 * control.y - 2.0 * orig.y
       let A = 4.0 * (ax * ax + ay * ay)
       let B = 4.0 * (ax * bx + ay * by)
       let C = bx * bx + by * by
       let Sabc = 2.0 * sqrt(A+B+C)
       let A_2 = sqrt(A)
       let A_32 = 2.0 * A * A_2
       let C_2 = 2.0 * sqrt(C)
       let BA = B / A_2

       (A_32 * Sabc + A_2 * B * (Sabc - C_2) + (4.0 * C * A - B * B) * log((2.0 * A_2 + BA + Sabc) / (BA + C_2))) / (4.0 * A_32);

    let straightLength orig dest =
        let square x = x * x
        (square (orig.x - dest.x)) + (square (orig.y - dest.y)) |> sqrt

    let length segment = 
        match segment with
            | Straight (orig, dest) -> straightLength orig dest
            | QuadraticBezier (orig, control, dest) -> quadraticBezierLength orig control dest

    let segmentAt segmentChain location =
        let totalLength = List.sumBy length segmentChain
        let hitAt = location * totalLength
        let runningTotals = 
            segmentChain
            |> List.fold (fun (lengthSoFar, resultList) segment -> (length segment + lengthSoFar, (segment, length segment + lengthSoFar)::resultList)) (0.0, [])
            |> snd
            |> List.rev 
        let choice = runningTotals |> List.skipWhile (fun (segment, runningTotal) -> runningTotal < hitAt) 
        match choice with
            | head :: _ -> head
            | _ -> failwith "Need to extend segment into line and walk out. fails if this is a curve."

    let slope orig dest = 
        match dest.x - orig.x with
            | 0.0 -> None
            | _ -> Some <| (dest.y - orig.y) / (dest.x - orig.x)

    let pointSlope orig dest = 
        slope orig dest |> Option.map (fun m x -> m * (x - dest.x) + dest.y)

    let evaluate orig dest x =
        pointSlope orig dest |> Option.map (fun f -> f x)

    let pointOnStraightSegment orig dest location = 
        let dx = dest.x - orig.x
        let dy = dest.y - orig.y
        let newX = orig.x + dx*location
        match evaluate orig dest newX with
            | None -> {x = orig.x; y = orig.y + dy*location}
            | Some y -> {x = newX; y = y}

    let pointOnSegmentChain (schain: SegmentChain) (location: Location) : Segment * Point = failwith ""

    let solveLinePerpendicular (location: Location) (segment: Segment) =
        match segment with
            | QuadraticBezier(_, _, _) -> failwith "oof"
            | Straight(orig, dest) -> 
                match slope orig dest with
                    // Vertical lines become horizontal lines with slope=0
                    | None -> Sloped(pointOnStraightSegment orig dest location, 0.0)
                    // Horizontal lines become vertical lines
                    | Some 0.0 -> pointOnStraightSegment orig dest location |> Point.x |> Vertical
                    // All other lines have m -> -1/m
                    | Some m -> Sloped(pointOnStraightSegment orig dest location, -1.0 / m)

    let solveLineExtendSegment segment = 
        match segment with 
            | QuadraticBezier(_,_,_) -> failwith "oof"
            | Straight(orig, dest) -> 
                match slope orig dest with
                    | None -> Vertical orig.x
                    | Some m -> Sloped (orig, m)

    let solveLineVerticalThroughX location segment =
        match segment with 
            | QuadraticBezier(_,_,_) -> failwith "oof"
            | Straight(orig, dest) -> pointOnStraightSegment orig dest location |> Point.x |> Vertical

    let solveLineHorizontalThroughY location segment =
        match segment with
            | QuadraticBezier(_,_,_) -> failwith "oof"
            | Straight(orig, dest) -> Sloped(pointOnStraightSegment orig dest location, 0.0)






    let mergeSegmentChains chain1 chain2 = failwith ""


module TestIt =
    open Solve
    let chain = [Straight ({x = 0.0; y= 0.0;}, {x = 2.0; y = 0.0;}); Straight ({x = 2.0; y = 0.0;}, {x = 3.0; y = 1.0;})]


    let computed = segmentAt chain 0.8


    // let test1 =  pointOnSegment (point 0.0 0.0) (point 1.0 2.0) 0.5 

    //

    // type SolveContext = {
    //    PointContext: Dictionary<L.Point, Point>; 
    //    SegmentContext: Dictionary<L.Segment, Segment>;
    // }

    // let makeContext () = {PointContext = new Dictionary<L.Point, Point>(); SegmentContext = new Dictionary<L.Segment, Segment>()}

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





