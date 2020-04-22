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

