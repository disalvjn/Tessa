namespace Tessa.Eval.Types

open Tessa.Language
open Tessa.Solve.Types
open Tessa.Parse

// todo: make sure Parse and Solve both return Result
module EvalTypes = 
    module P = Parse
    module L = Language
    module S = SolveTypes

    type CellName = CellName of string
    type PolygonName = PolygonName of CellName * int

    type PrimitiveProcedure = 
        | AddNumber
        | Square
        | IsoscelesRight
        | Assign
        | RecordBuilder 
        | RecordAccess
        | ArrayBuilder 
        | LinkPoints 
        | Perpendicular 
        | At 
        | C4Clockwise
        | ApplyOp 
        | Snip
        | Intersect 
        | Draw
        | Lambda

    // todo: Need to pipe Lex pos into Parse so I can add positions here
    type EvalError =
        | UndefinedVariable of var: string * message: string
        | AddingNonNumbers of Exp list
        | ApplyingNonFunction of Exp
        | UnbalancedParenExtraClose
        | AssignError
        | RecordBuildingError
        | RecordAccessError of field: string * record: Exp option
        | ArrayAssignmentUnequalCardinalities of Exp list * Exp list
        | NotASymbol of Exp
        | NotALinkablePointOrSegment of Exp
        | LinkingMoreThanTwoPointsOrSegments of Exp list
        | WrongArgumentsToPerpendicular of Exp list
        | WrongArgumentsToIntersect of Exp list
        | NotASegment of Exp 
        | NotANumber of Exp
        | NotAPoint of Exp
        | NotAnOperation of Exp
        | WrongArgumentsToAt of Exp list
        | WrongArgumentsToRotation of Exp list
        | WrongArgumentsToApplyOp of Exp list
        | WrongArgumentsToSnip of Exp list
        | WrongArgumentsToDraw of Exp list

    and Exp =
        | Number of float
        | Identifier of string
        | PrimitiveProcedure of PrimitiveProcedure
        | Quote of P.StackCommand
        | Record of Map<string, Exp>
        | Array of Exp list
        // | Lambda
        | GeoExp of GeoExp
        // An operation by itself does nothing, it's solved in the context of a point.
        // But, they're still first class citizens. One solution is GeoExp having two branches,
        // one for shapes and one for the operation.
        | LOperation of L.Operation

    and PolygonIndex = PolygonIndex of L.Operation option list

    and GeoExp =
        | LPoint of L.Point
        | LSegment of L.Segment
        | LLine of L.Line
        | Polygon of centroid: L.Point * name: PolygonName * index: PolygonIndex

    let toNumber exp = 
        match exp with 
        | Number n -> Ok n 
        | other -> Error other 

    type Operation =
        | Primitive of PrimitiveProcedure
        // | Fun

    type OperationState = 
        | Empty 
        | EmptyAcceptNext
        | Op of Operation

    type EvaluatorMessage =
        | AugmentEnvironment of Map<string, Exp>
        | DrawGeo of string * GeoExp


    type DrawMap = Map<CellName, GeoExp list>
    type Environment = Map<string, Exp>


    type SolvedShape = 
        | Point of S.PointId * string 
        | Segment of S.SegmentId * string
        | Line of S.Line

    type Runtime = {
        drawMap: DrawMap;
        environment: Environment;
        geoCanon: S.CanonicizerState;
        solvedShapes: Map<CellName, SolvedShape list>;
    }

    type EvalResult = {
        value: Exp option;
        runtime: Runtime;
        error: EvalError option;
    }
