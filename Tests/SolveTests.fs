namespace Tessa.SolveTests

open System
open Xunit
open Tessa.Solve

module SolveTests = 
    module S = Solve

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

