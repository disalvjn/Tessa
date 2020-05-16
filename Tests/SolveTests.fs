namespace Tessa.Solve.Tests

open System
open Xunit
open Tessa.Solve.Shapes
open Tessa.Solve.Types
open Tessa.Solve.Polygons
open Tessa.Language
open Tessa.Util

module SolveTests = 
    open SolveShapesInternal
    open SolvePolygons
    open SolveTypes
    module L = Language
    open Util

    let fromResult = function
        | Error e -> failwith (sprintf "%A" e)
        | Ok o -> o

    let AssertPointEqual (actual: Point) (expected: Point) =
        let eps = 0.01
        Assert.InRange(actual.x, expected.x - eps, expected.x + eps)
        Assert.InRange(actual.y, expected.y - eps, expected.y + eps)

    let AssertSegmentEqual (Straight(orig, dest)) (Straight(orig2, dest2)) =
        AssertPointEqual orig orig2
        AssertPointEqual dest dest2
    
    let AssertSegmentChainEqual expected actual = 
        Assert.Equal(List.length expected, List.length actual)
        List.zip expected actual |> List.iter (uncurry AssertSegmentEqual)

    let p x y = {x = x; y = y;}

    let toSegments points  =
        Seq.zip points (List.tail points)
        |> Seq.fold (fun segList (p1, p2) -> Straight(p1, p2) :: segList) []
        |> Seq.toList
        |> List.rev

    [<Fact>]
    let ``solveLineVerticalThroughX on Straight Segment`` () =
        let segment = Straight({x= 0.0; y = 0.0;}, {x = 1.0; y = 1.0;})
        let solved = solveLineVerticalThroughX 0.5 [segment]
        Assert.Equal(Ok <| Vertical(0.5), solved)

    [<Fact>]
    let ``solveLineHorizontalThroughY on Straight Segment`` () =
        let segment = Straight({x= 0.0; y = 0.0;}, {x = 1.0; y = 1.0;})
        let solved = solveLineHorizontalThroughY 0.5 [segment]
        Assert.Equal(Ok <| Sloped({x = 0.5; y = 0.5;}, 0.0), solved)

    [<Fact>]
    let ``solveLineExtendSegment Vertical Straight Segment`` () =
        let segment = Straight({x = 0.0; y = 0.0;}, {x = 0.0; y = 10.0;})
        let solved = solveLineExtendSegment [segment]
        Assert.Equal(Ok <| Vertical(0.0), solved)

    [<Fact>]
    let ``solveLineExtendSegment Sloped Straight Segment`` () =
        let segment = Straight({x = 0.0; y = 0.0;}, {x = 1.0; y = 2.0;})
        let solved = solveLineExtendSegment [segment]
        Assert.Equal(Ok <| Sloped({x = 0.0; y = 0.0;}, 2.0), solved)

    [<Fact>]
    let ``solveLinePerpendicular Sloped Straight Segment`` () = 
        let segment = Straight({x = 0.0; y = 0.0;}, {x = 1.0; y = 2.0;})
        let solved = solveLinePerpendicular 0.5 [segment]
        Assert.Equal(Ok <| Sloped({x = 0.5; y = 1.0;}, -1.0/2.0), solved)

    [<Fact>]
    let ``solveLinePerpendicular Vertical Straight Segment`` () =
        let segment = Straight({x = 0.0; y = 0.0;}, {x = 0.0; y = 10.0;})
        let solved = solveLinePerpendicular 0.7 [segment]
        Assert.Equal(Ok <| Sloped({x = 0.0; y = 7.0;}, 0.0), solved)

    [<Fact>]
    let ``solveLinePerpendicular Horizontal Straight Segment`` () =
        let segment = Straight({x = 0.0; y = 0.0;}, {x = 10.0; y = 0.0;})
        let solved = solveLinePerpendicular 0.7 [segment]
        Assert.Equal(Ok <| Vertical(7.0), solved)

    [<Fact>]
    let ``pointOnSegmentChain`` () =
        let segment (x1, y1) (x2, y2) = Straight({x = float x1; y = float y1;}, {x = float x2; y = float y2;})
        let s1, s2, s3, s4, s5 = segment (0, 0) (2, 0), segment (2, 0) (3, 0), segment (3, 0) (5, 0), segment (5, 0) (7, 0), segment (7, 0) (8, 0)
        let chain = [s1; s2; s3; s4; s5]

        Assert.Equal(fromResult <| pointOnSegmentChain 0.0 chain, (s1, {Point.x = 0.0; Point.y = 0.0;}))
        
        Assert.Equal(fromResult <| pointOnSegmentChain 0.25 chain, (s1, {Point.x = 2.0; Point.y = 0.0;}))

        Assert.Equal(fromResult <| pointOnSegmentChain 0.5 chain, (s3, {Point.x = 4.0; Point.y = 0.0;}))

        Assert.Equal(fromResult <| pointOnSegmentChain 0.75 chain, (s4, {Point.x = 6.0; Point.y = 0.0;}))

        Assert.Equal(fromResult <| pointOnSegmentChain 1.0 chain, (s5, {Point.x = 8.0; Point.y = 0.0;}))

        Assert.Equal(fromResult <| pointOnSegmentChain 2.0 chain, (s5, {Point.x = 16.0; Point.y = 0.0;}))

    [<Fact>]
    let ``Rotate around point`` () =

        let around = p 1.0 1.0
        let start = p 1.0 2.0

        AssertPointEqual (p 2.0 1.0) <| rotateAround start around L.Clockwise L.C4
        AssertPointEqual (p 0.0 1.0) <| rotateAround start around L.CounterClockwise L.C4
        AssertPointEqual (p 1.0 0.0) <| rotateAround start around L.Clockwise L.C2

        let fullCircle angle = 
            (fun s -> rotateAround s around L.Clockwise angle) >> (fun s -> rotateAround s around L.CounterClockwise angle)

        AssertPointEqual (p 3.0 4.0) <| fullCircle L.C6 (p 3.0 4.0)
        AssertPointEqual (p 3.0 4.0) <| fullCircle L.C3 (p 3.0 4.0)

    [<Fact>]
    let ``Solve Point at Intersection of Lines`` () =
        let verticalThrough1 = Vertical(1.0)
        let upDiag = Sloped(p 0.0 0.0, 1.0)
        let downDiag = Sloped(p 0.0 0.0, -1.0)

        let solve = fun l1 l2 -> fromResult <| solvePointLineIntersect l1 l2

        AssertPointEqual (p 1.0 1.0)  <| solve verticalThrough1 upDiag
        AssertPointEqual (p 1.0 -1.0) <| solve verticalThrough1 downDiag
        AssertPointEqual (p 0.0 0.0)  <| solve upDiag downDiag

    [<Fact>]
    let ``Solve Segment Snipped`` () =
        let chain1 = toSegments [p -3.0 4.0; p -1.0 2.0; p -3.0 1.0; p -2.0 0.0; p -1.0 1.0; p 1.0 -1.0; p 1.0 -3.0]
        let chain2 = toSegments [p 3.0 3.0; p 1.0 2.0; p 1.0 1.0; p -1.0 -1.0; p -1.0 -2.0]

        // AssertPointEqual (List.head chain1 |> fun (Straight (orig, dest)) -> orig) (p -3.0 4.0)
        // AssertPointEqual (List.head chain1 |> fun (Straight (orig, dest)) -> dest) (p -1.0 2.0)
        // AssertPointEqual (List.head (List.tail chain1) |> fun (Straight (orig, dest)) -> orig) (p -1.0 2.0)

        let snippedChain1 = solveSegmentChainSnipped chain1 chain2
        Assert.Equal(5, List.length snippedChain1)
        AssertSegmentEqual (List.last snippedChain1) <| Straight(p -1.0 1.0, p 0.0 0.0)

        let snippedChain2 = solveSegmentChainSnipped chain2 chain1
        Assert.Equal(3, List.length snippedChain2)
        AssertSegmentEqual (List.last snippedChain2) <| Straight(p 1.0 1.0, p 0.0 0.0)


    [<Fact>]
    let ``Solve Segment Perpendicular`` () =
        let beginFrom = toSegments [p -1.0 1.0 ; p 1.0 1.0]
        let snipTo1 = toSegments [p -1.0 5.0; p 1.0 5.0; p -1.0 6.0]
        let snipTo2 = toSegments [p -1.0 0.0; p 1.0 0.0; p -1.0 -1.0]

        let perpTo1 = solveSegmentPerpendicular 0.5 beginFrom snipTo1
        let perpTo2 = solveSegmentPerpendicular 0.5 beginFrom snipTo2

        AssertSegmentEqual (fromResult perpTo1) <| Straight(p 0.0 1.0, p 0.0 5.0)
        AssertSegmentEqual (fromResult perpTo2) <| Straight(p 0.0 1.0, p 0.0 0.0)

    [<Fact>]
    let ``Atomize Segment`` () = 
        let segment = List.head <| toSegments [p 0.0 0.0; p 3.0 0.0;]
        // -|_|- type of shape
        let chain = toSegments [p 1.0 1.0; p 1.0 -1.0; p 2.0 -1.0; p 2.0 1.0; p 3.0 1.0; p 3.0 -1.0;]

        let atoms = atomizeSegment segment chain |> List.sortBy (fun (Straight(orig, dest)) -> orig.x)

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

        let segments = atomizeSegments segments
        Assert.Equal(12, List.length segments) // there are 12 segments.

    [<Fact>]
    let ``Atomize Isosceles with one cross`` () = 
        let segments = 
            [
            Straight ({x = 0.3535533905932738; y = 0.3535533905932738},{x = 0.5303300858899107; y = 0.7071067811865476}); 
            Straight ({x = 0.0; y = 0.0},{x = 0.0; y = 0.7071067811865476}); 
            Straight ({x = 0.0; y = 0.7071067811865476},{x = 0.7071067811865476; y = 0.7071067811865476}); 
            Straight ({x = 0.7071067811865476; y = 0.7071067811865476},{x = 0.0; y = 0.0})] 
        
        let atoms = atomizeSegments segments

        Assert.Equal(6, List.length atoms)
    let intsToSegmentsDividedTriangle points  =
        let pointMap = [
            (1, {x = 0.0; y = 0.0;});
            (2, {x = 1.0; y = 0.0;});
            (3, {x = 0.5; y = 1.0;});
            (4, {x = 0.5; y = 0.0;})
        ]

        let numToPoint = Map.ofList pointMap

        Seq.zip points (List.tail points)
        |> Seq.fold (fun segList (p1, p2) -> Straight(Map.find p1 numToPoint, Map.find p2 numToPoint) :: segList) []
        |> Seq.toList
        |> List.rev

    let joinToPolygons atoms = 
        joinToPolygonsAsSegments atoms 
        |> Set.ofList 
        |> Set.map (fun polygon -> List.sortBy (fun (Straight(p, q)) -> p) polygon)

    [<Fact>]
    let ``Join To Polygons Simple Triangle`` () = 
        let atoms = intsToSegmentsDividedTriangle [1;2;3;1]
        let joined = Set.toList <| joinToPolygons atoms 
        // todo: make this test more worthwhile
        Assert.Equal(1, List.length joined)
        Assert.Equal(3, List.length <| List.head joined)
        // let expected = Set.ofList [intsToSegments [1;2;3;1]]
        // Assert.Equal<Set<Segment list>>(expected, joined)

    [<Fact>]
    let ``Join To Polygons Simple Triangle With Divider`` () = 
        let atoms = intsToSegmentsDividedTriangle [1;2;4;1] @  intsToSegmentsDividedTriangle [2;3;4]
        let joined = Set.toList <| joinToPolygons atoms
        // todo: make this test more worthwhile
        Assert.Equal(2, List.length joined)
        Assert.Equal(3, List.length <| List.head joined)
        Assert.Equal(3, List.length <| List.last joined)
        // let expected = Set.ofList [intsToSegments [1;2;4;1]; intsToSegments [2;3;4;2]]
        // Assert.Equal<Set<Segment list>>(expected, joined)

    [<Fact>]
    let ``Join To Polygons Simple Non-Closed Shape`` () = 
        let atoms = intsToSegmentsDividedTriangle [1;2;4;1] @  intsToSegmentsDividedTriangle [2;3]
        let joined = Set.toList <| joinToPolygons atoms 
        // todo: make this test more worthwhile
        Assert.Equal(1, List.length joined)
        Assert.Equal(3, List.length <| List.head joined)
        // let expected = Set.ofList [intsToSegments [1;2;4;1];]
        // Assert.Equal<Set<Segment list>>(expected, joined)

    let intsToSegmentsSquareCross points  =
        let pointMap = [
            (1, {x = 0.0; y = 0.0;});
            (2, {x = 1.0; y = 0.0;});
            (3, {x = 1.0; y = 1.0;});
            (4, {x = 0.0; y = 1.0;});
            (5, {x = 0.5; y = 0.5});
        ]

        let numToPoint = Map.ofList pointMap

        Seq.zip points (List.tail points)
        |> Seq.fold (fun segList (p1, p2) -> Straight(Map.find p1 numToPoint, Map.find p2 numToPoint) :: segList) []
        |> Seq.toList
        |> List.rev
    
    [<Fact>]
    let ``Join To Polygons Square With Cross`` () =
        let joined = [[1;2;3;4;1]; [1;3;]; [2;4;]] |> List.collect intsToSegmentsSquareCross |> atomizeSegments |> joinToPolygons |> Set.toList
            
        Assert.Equal(4, List.length joined)

    [<Fact>]
    let ``Join To Polygons Inside Path`` () =
        let joined = [[1;2;3;4;1]; [1;3;];] |> List.collect intsToSegmentsDividedTriangle |> atomizeSegments |> joinToPolygons |> Set.toList
            
        Assert.Equal(2, List.length joined)

    [<Fact>]
    let ``Join To Polygons Isosceles Triangle Split`` () = 
        let atoms = [
            Straight ({x = 0.35; y = 0.35},{x = 0.0; y = 0.71}); 
            Straight ({x = 0.0; y = 0.0},{x = 0.0; y = 0.71}); 
            Straight ({x = 0.0; y = 0.71},{x = 0.71; y = 0.71}); 
            Straight ({x = 0.35; y = 0.35},{x = 0.0; y = 0.0}); 
            Straight ({x = 0.71; y = 0.71},{x = 0.35; y = 0.35})];
        let joined = joinToPolygons atoms |> Set.toList
        Assert.Equal(2, List.length joined)

    [<Fact>]
    let ``Join To Polygons Isosceles With Two Splits`` () = 
        let atoms = [
            Straight ({x = 0.53; y = 0.71},{x = 0.35; y = 0.35}); 
            Straight ({x = 0.0; y = 0.18},{x = 0.35; y = 0.35}); 
            Straight ({x = 0.0; y = 0.0},{x = 0.0; y = 0.18}); 
            Straight ({x = 0.0; y = 0.18},{x = 0.0; y = 0.71}); 
            Straight ({x = 0.0; y = 0.71},{x = 0.53; y = 0.71}); 
            Straight ({x = 0.53; y = 0.71},{x = 0.71; y = 0.71}); 
            Straight ({x = 0.35; y = 0.35},{x = 0.0; y = 0.0}); 
            Straight ({x = 0.71; y = 0.71},{x = 0.35; y = 0.35})]
        let joined = joinToPolygons atoms |> Set.toList 
        Assert.Equal(3, List.length joined)

    [<Fact>]
    let ``Join To Polygons Isosceles With Multiple Splits`` () = 
        let atoms = [
            Straight ({x = 0.0; y = 0.70711},{x = 0.04419; y = 0.66291}); 
            Straight ({x = 0.0; y = 0.42426},{x = 0.04419; y = 0.66291}); 
            Straight ({x = 0.04419; y = 0.66291},{x = 0.28284; y = 0.70711}); 
            Straight ({x = 0.35355; y = 0.35355},{x = 0.0; y = 0.11785}); 
            Straight ({x = 0.35355; y = 0.35355},{x = 0.58926; y = 0.70711}); 
            Straight ({x = 0.0; y = 0.0},{x = 0.0; y = 0.11785}); 
            Straight ({x = 0.0; y = 0.11785},{x = 0.0; y = 0.42426}); 
            Straight ({x = 0.0; y = 0.42426},{x = 0.0; y = 0.70711}); 
            Straight ({x = 0.0; y = 0.70711},{x = 0.28284; y = 0.70711}); 
            Straight ({x = 0.28284; y = 0.70711},{x = 0.58926; y = 0.70711}); 
            Straight ({x = 0.58926; y = 0.70711},{x = 0.70711; y = 0.70711}); 
            Straight ({x = 0.35355; y = 0.35355},{x = 0.0; y = 0.0}); 
            Straight ({x = 0.35355; y = 0.35355},{x = 0.0; y = 0.0}); 
            Straight ({x = 0.70711; y = 0.70711},{x = 0.35355; y = 0.35355}); 
            Straight ({x = 0.70711; y = 0.70711},{x = 0.35355; y = 0.35355})]
        // atoms |> List.collect (fun (Straight(p, q)) -> [p; q]) |> List.distinct |> failAndPrint
        let joined = joinToPolygons atoms |> Set.toList
        // [for poly in joined do for (Straight(p, q)) in poly do [p.x; p.y; q.x; q.y]] |> List.concat |> List.distinct |> List.sort |> failAndPrint 
        Assert.Equal(5, List.length joined) 
