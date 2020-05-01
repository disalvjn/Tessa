namespace Tessa.View.Types 

open System
open Tessa.Language
open Tessa.Solve

// Precondition: everything fits in unit square before ops?
module ViewTypes =
    module L = Language
    module S = Solve

    type ViewMode =
        | Preview of label: string
        | Drawn of label: string

    // find bounding box, scale up *, move to middle +

    type Point = {x: int; y: int; label: string}
    type Segment = {orig: Point; dest: Point; label: string}

    type ViewShape = 
        | ViewPoint of S.Point
        | ViewSegment of S.SegmentChain
        | ViewLine of S.Line

    type View = {
        shape: ViewShape;
        mode: ViewMode;
    }

    