namespace Tessa.SolveTests

open System
open Xunit
open Tessa.Solve
open Tessa.Language
open Tessa.Util

module SolveTests = 
    module S = Solve
    module L = Language
    open Util

    let fromResult = function
        | Error e -> failwith (sprintf "%A" e)
        | Ok o -> o

    let AssertPointEqual (actual: S.Point) (expected: S.Point) =
        let eps = 0.01
        Assert.InRange(actual.x, expected.x - eps, expected.x + eps)
        Assert.InRange(actual.y, expected.y - eps, expected.y + eps)

    let AssertSegmentEqual (S.Straight(orig, dest)) (S.Straight(orig2, dest2)) =
        AssertPointEqual orig orig2
        AssertPointEqual dest dest2
    
    let AssertSegmentChainEqual expected actual = 
        Assert.Equal(List.length expected, List.length actual)
        List.zip expected actual |> List.iter (uncurry AssertSegmentEqual)

    let p x y = {S.x = x; S.y = y;}

    let toSegments points  =
        Seq.zip points (List.tail points)
        |> Seq.fold (fun segList (p1, p2) -> S.Straight(p1, p2) :: segList) []
        |> Seq.toList
        |> List.rev

    [<Fact>]
    let ``solveLineVerticalThroughX on Straight Segment`` () =
        let segment = S.Straight({x= 0.0; y = 0.0;}, {x = 1.0; y = 1.0;})
        let solved = S.solveLineVerticalThroughX 0.5 [segment]
        Assert.Equal(Ok <| S.Vertical(0.5), solved)

    [<Fact>]
    let ``solveLineHorizontalThroughY on Straight Segment`` () =
        let segment = S.Straight({x= 0.0; y = 0.0;}, {x = 1.0; y = 1.0;})
        let solved = S.solveLineHorizontalThroughY 0.5 [segment]
        Assert.Equal(Ok <| S.Sloped({x = 0.5; y = 0.5;}, 0.0), solved)

    [<Fact>]
    let ``solveLineExtendSegment Vertical Straight Segment`` () =
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 0.0; y = 10.0;})
        let solved = S.solveLineExtendSegment [segment]
        Assert.Equal(Ok <| S.Vertical(0.0), solved)

    [<Fact>]
    let ``solveLineExtendSegment Sloped Straight Segment`` () =
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 1.0; y = 2.0;})
        let solved = S.solveLineExtendSegment [segment]
        Assert.Equal(Ok <| S.Sloped({x = 0.0; y = 0.0;}, 2.0), solved)

    [<Fact>]
    let ``solveLinePerpendicular Sloped Straight Segment`` () = 
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 1.0; y = 2.0;})
        let solved = S.solveLinePerpendicular 0.5 [segment]
        Assert.Equal(Ok <| S.Sloped({x = 0.5; y = 1.0;}, -1.0/2.0), solved)

    [<Fact>]
    let ``solveLinePerpendicular Vertical Straight Segment`` () =
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 0.0; y = 10.0;})
        let solved = S.solveLinePerpendicular 0.7 [segment]
        Assert.Equal(Ok <| S.Sloped({x = 0.0; y = 7.0;}, 0.0), solved)

    [<Fact>]
    let ``solveLinePerpendicular Horizontal Straight Segment`` () =
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 10.0; y = 0.0;})
        let solved = S.solveLinePerpendicular 0.7 [segment]
        Assert.Equal(Ok <| S.Vertical(7.0), solved)

    [<Fact>]
    let ``pointOnSegmentChain`` () =
        let segment (x1, y1) (x2, y2) = S.Straight({x = float x1; y = float y1;}, {x = float x2; y = float y2;})
        let s1, s2, s3, s4, s5 = segment (0, 0) (2, 0), segment (2, 0) (3, 0), segment (3, 0) (5, 0), segment (5, 0) (7, 0), segment (7, 0) (8, 0)
        let chain = [s1; s2; s3; s4; s5]

        Assert.Equal(fromResult <| S.pointOnSegmentChain 0.0 chain, (s1, {S.Point.x = 0.0; S.Point.y = 0.0;}))
        
        Assert.Equal(fromResult <| S.pointOnSegmentChain 0.25 chain, (s1, {S.Point.x = 2.0; S.Point.y = 0.0;}))

        Assert.Equal(fromResult <| S.pointOnSegmentChain 0.5 chain, (s3, {S.Point.x = 4.0; S.Point.y = 0.0;}))

        Assert.Equal(fromResult <| S.pointOnSegmentChain 0.75 chain, (s4, {S.Point.x = 6.0; S.Point.y = 0.0;}))

        Assert.Equal(fromResult <| S.pointOnSegmentChain 1.0 chain, (s5, {S.Point.x = 8.0; S.Point.y = 0.0;}))

        Assert.Equal(fromResult <| S.pointOnSegmentChain 2.0 chain, (s5, {S.Point.x = 16.0; S.Point.y = 0.0;}))

    [<Fact>]
    let ``Rotate around point`` () =

        let around = p 1.0 1.0
        let start = p 1.0 2.0

        AssertPointEqual (p 2.0 1.0) <| S.rotateAround start around L.Clockwise L.C4
        AssertPointEqual (p 0.0 1.0) <| S.rotateAround start around L.CounterClockwise L.C4
        AssertPointEqual (p 1.0 0.0) <| S.rotateAround start around L.Clockwise L.C2

        let fullCircle angle = 
            (fun s -> S.rotateAround s around L.Clockwise angle) >> (fun s -> S.rotateAround s around L.CounterClockwise angle)

        AssertPointEqual (p 3.0 4.0) <| fullCircle L.C6 (p 3.0 4.0)
        AssertPointEqual (p 3.0 4.0) <| fullCircle L.C3 (p 3.0 4.0)

    [<Fact>]
    let ``Solve Point at Intersection of Lines`` () =
        let verticalThrough1 = S.Vertical(1.0)
        let upDiag = S.Sloped(p 0.0 0.0, 1.0)
        let downDiag = S.Sloped(p 0.0 0.0, -1.0)

        let solve = fun l1 l2 -> fromResult <| S.solvePointLineIntersect l1 l2

        AssertPointEqual (p 1.0 1.0)  <| solve verticalThrough1 upDiag
        AssertPointEqual (p 1.0 -1.0) <| solve verticalThrough1 downDiag
        AssertPointEqual (p 0.0 0.0)  <| solve upDiag downDiag

    [<Fact>]
    let ``Solve Segment Snipped`` () =
        let chain1 = toSegments [p -3.0 4.0; p -1.0 2.0; p -3.0 1.0; p -2.0 0.0; p -1.0 1.0; p 1.0 -1.0; p 1.0 -3.0]
        let chain2 = toSegments [p 3.0 3.0; p 1.0 2.0; p 1.0 1.0; p -1.0 -1.0; p -1.0 -2.0]

        // AssertPointEqual (List.head chain1 |> fun (S.Straight (orig, dest)) -> orig) (p -3.0 4.0)
        // AssertPointEqual (List.head chain1 |> fun (S.Straight (orig, dest)) -> dest) (p -1.0 2.0)
        // AssertPointEqual (List.head (List.tail chain1) |> fun (S.Straight (orig, dest)) -> orig) (p -1.0 2.0)

        let snippedChain1 = S.solveSegmentChainSnipped chain1 chain2
        Assert.Equal(5, List.length snippedChain1)
        AssertSegmentEqual (List.last snippedChain1) <| S.Straight(p -1.0 1.0, p 0.0 0.0)

        let snippedChain2 = S.solveSegmentChainSnipped chain2 chain1
        Assert.Equal(3, List.length snippedChain2)
        AssertSegmentEqual (List.last snippedChain2) <| S.Straight(p 1.0 1.0, p 0.0 0.0)


    [<Fact>]
    let ``Solve Segment Perpendicular`` () =
        let beginFrom = toSegments [p -1.0 1.0 ; p 1.0 1.0]
        let snipTo1 = toSegments [p -1.0 5.0; p 1.0 5.0; p -1.0 6.0]
        let snipTo2 = toSegments [p -1.0 0.0; p 1.0 0.0; p -1.0 -1.0]

        let perpTo1 = S.solveSegmentPerpendicular 0.5 beginFrom snipTo1
        let perpTo2 = S.solveSegmentPerpendicular 0.5 beginFrom snipTo2

        AssertSegmentEqual (fromResult perpTo1) <| S.Straight(p 0.0 1.0, p 0.0 5.0)
        AssertSegmentEqual (fromResult perpTo2) <| S.Straight(p 0.0 1.0, p 0.0 0.0)

    [<Fact>]
    let ``Atomize Segment`` () = 
        let segment = List.head <| toSegments [p 0.0 0.0; p 3.0 0.0;]
        // -|_|- type of shape
        let chain = toSegments [p 1.0 1.0; p 1.0 -1.0; p 2.0 -1.0; p 2.0 1.0; p 3.0 1.0; p 3.0 -1.0;]

        let atoms = S.atomizeSegment segment chain |> List.sortBy (fun (S.Straight(orig, dest)) -> orig.x)

        let expected = toSegments [p 0.0 0.0; p 1.0 0.0; p 2.0 0.0; p 3.0 0.0];
        AssertSegmentChainEqual expected atoms

    [<Fact>]
    let ``Atomize Segments`` () = 
        // square with a plus through the middle 
        let segments = 
            // The square
            toSegments [p 0.0 0.0; p 2.0 0.0; p 2.0 2.0; p 0.0 2.0; p 0.0 0.0;] 
            // Horizontal Line through middle
            @ toSegments [p 0.0 1.0; p 2.0 1.0;] 
            // Vertical Line through middle
            @ toSegments [p 1.0 2.0; p 1.0 0.0;]

        let (segmentIds, canon) = S.atomizeSegments segments
        Assert.Equal(9, canon.nextId) // there are nine unique points in the plus-square 
        Assert.Equal(12, List.length segmentIds) // there are 12 segments.

    [<Fact>]
    let ``Closed 1`` () =
        let isClosed ps = ps |> List.map (fun (x,y) -> Set.ofList [S.PointId x; S.PointId y]) |> Set.ofList |> S.closed

        Assert.Equal(true, Option.isNone <| isClosed [(1,2); (2, 3);])
        Assert.Equal(true, Option.isNone <| isClosed [(1,2)])
        Assert.Equal(true, Option.isNone <| isClosed [(1,3)])
        Assert.Equal(true, Option.isSome <| isClosed [(1,2); (1,4); (2,4)])
        Assert.Equal(true, Option.isSome <| isClosed [(2, 3); (3, 4); (4, 2)])
        Assert.Equal(true, Option.isSome <| isClosed [(1,2);(1,4);(2,3);(4,3)])

    let toSegmentIds points  =
        Seq.zip points (List.tail points)
        |> Seq.fold (fun segList (p1, p2) -> S.SegmentId(S.PointId p1, S.PointId p2) :: segList) []
        |> Seq.toList
        |> List.rev

    let joinToPolygons = S.joinToPolygons >> Set.ofList

    [<Fact>]
    let ``Join To Polygons Simple Triangle`` () = 
        let atoms = toSegmentIds [1;2;3;1]
        let joined = joinToPolygons atoms
        failAndPrint joined
        ()
        // let expected = Set.ofList [Set.ofList <| toSegmentIds [1;2;3;1]]
        // Assert.Equal<Set<Set<Set<S.PointId>>>>(expected, joined)

    [<Fact>]
    let ``Join To Polygons Simple Triangle With Divider`` () = 
        let atoms = toSegmentIds [1;2;4;1] @  toSegmentIds [2;3;4]
        let joined = joinToPolygons atoms
        failAndPrint joined
        ()
    //     let expected = Set.ofList [Set.ofList <| toSegmentIds [1;2;4;1]; Set.ofList <| toSegmentIds [2;3;4;2]]
    //     Assert.Equal<Set<Set<S.SegmentId>>>(expected, joined)





        


