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

    type Point = {x: float; y: float;}
    type Segment = {orig: Point; dest: Point;}

    type ViewShape = 
        | ViewPoint of S.Point
        | ViewSegment of S.SegmentChain
        | ViewLine of S.Line

    type View = {
        viewShape: ViewShape;
        viewMode: ViewMode;
    }

    type DrawTargets = {
        topLeft: float * float; 
        width: float;
        height: float;
    }

    type LineStyle = 
        | Solid 
        | Dotted

    type LineWidth =
        | ExtraThin
        | Thin 
        | Medium 
        | Thick
        | ExtraThick

    type SegmentDrawOptions = {
        color: string;
        label: string;
        lineStyle: LineStyle;
        lineWidth: LineWidth;
        accentuatePoints: bool;
    }

    type PointStyle = 
        | Filled
        | Hollow

    type PointDrawOptions = {
        color: string;
        label: string; 
        pointStyle: PointStyle;
    }

    type DrawShape = 
        | DrawPoint of Point * PointDrawOptions
        | DrawSegment of Segment  * SegmentDrawOptions