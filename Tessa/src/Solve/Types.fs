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

    type PointId = PointId of int
    type SegmentId = SegmentId of PointId * PointId 

    // canonicalize points phase, eps = 0.001 maybe
    // this changes input to View

    type Polygon = {
        segments: SegmentId list;
        index: int list;
        centroid: PointId;
    }

    let mapPolygon f polygon = 
        {polygon with 
            centroid = f polygon.centroid;
            segments = List.map (fun (SegmentId(p, q)) -> SegmentId(f p, f q)) polygon.segments}

    type CanonicizerState = {
        epsilon: float;
        idToPoint: Map<PointId, Point>;
        pointToId: Map<Point, PointId>;
        nextId: int;
    }

    let mapPoints f canonState =
        {canonState with 
            idToPoint = Map.map (fun k v -> f v) canonState.idToPoint; 
            pointToId = Map.mapList (fun k v -> (f k, v)) canonState.pointToId |> Map.ofList}

    let mapPointIds f canonState = 
        {canonState with 
            pointToId = Map.map (fun k v -> f v) canonState.pointToId;
            idToPoint = Map.mapList (fun k v -> (f k, v)) canonState.idToPoint |> Map.ofList}

    let emptyCanonicizerState = {
        epsilon = 0.001;
        idToPoint = Map.empty; 
        pointToId = Map.empty;
        nextId = 0;
    }

