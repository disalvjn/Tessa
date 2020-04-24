namespace Tessa.SolveTests

open System
open Xunit
open Tessa.Solve
open Tessa.Language

module SolveTests = 
    module S = Solve
    module L = Language

    let AssertPointEqual (actual: S.Point) (expected: S.Point) =
        let eps = 0.01
        Assert.InRange(actual.x, expected.x - eps, expected.x + eps)
        Assert.InRange(actual.y, expected.y - eps, expected.y + eps)

    let AssertSegmentEqual (S.Straight(orig, dest)) (S.Straight(orig2, dest2)) =
        AssertPointEqual orig orig2
        AssertPointEqual dest dest2

    let p x y = {S.x = x; S.y = y;}

    let toSegments points  =
        Seq.zip points (List.tail points)
        |> Seq.fold (fun segList (p1, p2) -> S.Straight(p1, p2) :: segList) []
        |> Seq.toList
        |> List.rev

    [<Fact>]
    let ``solveLineVerticalThroughX on Straight Segment`` () =
        let segment = S.Straight({x= 0.0; y = 0.0;}, {x = 1.0; y = 1.0;})
        let solved = S.solveLineVerticalThroughX 0.5 segment
        Assert.Equal(S.Vertical(0.5), solved)

    // [<Fact>]
    // let ``solveLineVerticalThroughX on QuadraticBezier Segment`` () =
    //     Assert.True(false)

    [<Fact>]
    let ``solveLineHorizontalThroughY on Straight Segment`` () =
        let segment = S.Straight({x= 0.0; y = 0.0;}, {x = 1.0; y = 1.0;})
        let solved = S.solveLineHorizontalThroughY 0.5 segment
        Assert.Equal(S.Sloped({x = 0.5; y = 0.5;}, 0.0), solved)

    // [<Fact>]
    // let ``solveLineHorizontalThroughY on QuadraticBezier Segment`` () =
    //     Assert.True(false)

    [<Fact>]
    let ``solveLineExtendSegment Vertical Straight Segment`` () =
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 0.0; y = 10.0;})
        let solved = S.solveLineExtendSegment segment
        Assert.Equal(S.Vertical(0.0), solved)

    [<Fact>]
    let ``solveLineExtendSegment Sloped Straight Segment`` () =
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 1.0; y = 2.0;})
        let solved = S.solveLineExtendSegment segment
        Assert.Equal(S.Sloped({x = 0.0; y = 0.0;}, 2.0), solved)

    // [<Fact>]
    // let ``solveLineExtendSegment QuadraticBezier Segment`` () =
    //     Assert.True(false)

    [<Fact>]
    let ``solveLinePerpendicular Sloped Straight Segment`` () = 
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 1.0; y = 2.0;})
        let solved = S.solveLinePerpendicular 0.5 segment
        Assert.Equal(S.Sloped({x = 0.5; y = 1.0;}, -1.0/2.0), solved)

    [<Fact>]
    let ``solveLinePerpendicular Vertical Straight Segment`` () =
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 0.0; y = 10.0;})
        let solved = S.solveLinePerpendicular 0.7 segment
        Assert.Equal(S.Sloped({x = 0.0; y = 7.0;}, 0.0), solved)

    [<Fact>]
    let ``solveLinePerpendicular Horizontal Straight Segment`` () =
        let segment = S.Straight({x = 0.0; y = 0.0;}, {x = 10.0; y = 0.0;})
        let solved = S.solveLinePerpendicular 0.7 segment
        Assert.Equal(S.Vertical(7.0), solved)

    [<Fact>]
    let ``pointOnSegmentChain`` () =
        let segment (x1, y1) (x2, y2) = S.Straight({x = float x1; y = float y1;}, {x = float x2; y = float y2;})
        let s1, s2, s3, s4, s5 = segment (0, 0) (2, 0), segment (2, 0) (3, 0), segment (3, 0) (5, 0), segment (5, 0) (7, 0), segment (7, 0) (8, 0)
        let chain = [s1; s2; s3; s4; s5]

        let (as1, p1) = S.pointOnSegmentChain chain 0.0
        Assert.Equal({S.Point.x = 0.0; S.Point.y = 0.0;}, p1)
        Assert.Equal(s1, as1)

        let (as2, p2) = S.pointOnSegmentChain chain 0.25
        Assert.Equal({S.Point.x = 2.0; S.Point.y = 0.0;}, p2)
        Assert.Equal(s1, as2)

        let (as3, p3) = S.pointOnSegmentChain chain 0.5
        Assert.Equal({S.Point.x = 4.0; S.Point.y = 0.0;}, p3)
        Assert.Equal(s3, as3)

        let (as4, p4) = S.pointOnSegmentChain chain 0.75
        Assert.Equal({S.Point.x = 6.0; S.Point.y = 0.0;}, p4)
        Assert.Equal(s4, as4)

        let (as5, p5) = S.pointOnSegmentChain chain 1.0
        Assert.Equal({S.Point.x = 8.0; S.Point.y = 0.0;}, p5)
        Assert.Equal(s5, as5)

        let (as6, p6) = S.pointOnSegmentChain chain 2.0
        Assert.Equal({S.Point.x = 16.0; S.Point.y = 0.0;}, p6)
        Assert.Equal(s5, as6)

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
        let fromResult = function
            | Error e -> failwith e
            | Ok o -> o
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

        let snippedChain1 = S.solveSegmentSnipped chain1 chain2
        Assert.Equal(5, List.length snippedChain1)
        AssertSegmentEqual (List.last snippedChain1) <| S.Straight(p -1.0 1.0, p 0.0 0.0)

        let snippedChain2 = S.solveSegmentSnipped chain2 chain1
        Assert.Equal(3, List.length snippedChain2)
        AssertSegmentEqual (List.last snippedChain2) <| S.Straight(p 1.0 1.0, p 0.0 0.0)

    [<Fact>]
    let ``Solve Segment Perpendicular`` () =
        let beginFrom = toSegments [p -1.0 1.0 ; p 1.0 1.0]
        let snipTo1 = toSegments [p -1.0 5.0; p 1.0 5.0; p -1.0 6.0]
        let snipTo2 = toSegments [p -1.0 0.0; p 1.0 0.0; p -1.0 -1.0]

        let perpTo1 = S.solveSegmentPerpendicular 0.5 beginFrom snipTo1
        let perpTo2 = S.solveSegmentPerpendicular 0.5 beginFrom snipTo2

        AssertSegmentEqual perpTo1 <| S.Straight(p 0.0 1.0, p 0.0 5.0)
        AssertSegmentEqual perpTo2 <| S.Straight(p 0.0 1.0, p 0.0 0.0)


        


