namespace Tessa.View.Types 

open System
open Tessa.Language
open Tessa.Solve.Types

// Precondition: everything fits in unit square before ops?
module ViewTypes =
    module L = Language
    module S = SolveTypes

    type DrawTargets = {
        topLeft: float * float; 
        boundingWidth: float;
        boundingHeight: float;
        xMax: float; 
        yMax: float;
    }
    type PolygonDrawOptions = {
        color: string;
    }

    type PointDrawOptions = {
        color: string;
        label: string; 
    }

    type DrawShape = 
        | DrawPoint of (float * float) * PointDrawOptions
        | DrawPolygon of (float * float) list  * PolygonDrawOptions