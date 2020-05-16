namespace Tessa.Solve.Types

open System
open FSharp.Collections
open Tessa.Language 
open Tessa.Util
open System.Collections.Generic

// todo: Split into more submodules. Solve.Types, Solve.Line (for line solver) etc.?
module SolveTypes =
    module L = Language 

    type Point = {x: double; y: double;}
    // https://stackoverflow.com/a/26565842/10558918
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Point = 
        let x: Point -> double = fun p -> p.x
        let y: Point -> double = fun p -> p.y

    type Segment = 
        | Straight of Point * Point

    type SegmentChain = Segment list

    type Line = 
        | Vertical of x: double
        | Sloped of xy: Point * m: double

    type Location = double

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

    type Solver = {
        line: L.Line -> Result<Line, SolveError>;
        segment: L.Segment -> Result<SegmentChain, SolveError>;
        point: L.Point -> Result<Point, SolveError>;
    }

    type Polygon = {
        segments: Segment list;
        index: int list;
        centroid: Point;
    }

    let mapSegment f (Straight(p, q)) = Straight(f p, f q)

    let mapPointsPolygon f polygon = 
        {polygon with 
            centroid = f polygon.centroid;
            segments = List.map (fun (Straight(p, q)) -> Straight(f p, f q)) polygon.segments}

    let mapPointsPolygons f polygons = List.map (mapPointsPolygon f) polygons

    let allPoints polygons = List.collect (fun p -> List.collect (fun (Straight(p, q)) -> [p; q;]) p.segments) polygons

    let indexAppend ind polygons = List.map (fun p -> {p with index = ind @ p.index;}) polygons
