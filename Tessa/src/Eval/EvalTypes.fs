namespace Tessa.Eval.Types

open Tessa.Language
open Tessa.Solve
open Tessa.Parse

// todo: make sure Parse and Solve both return Result
module EvalTypes = 
    module P = Parse
    module L = Language
    module S = Solve

    type PrimitiveProcedure = 
        | AddNumber
        | Square
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

    and Exp =
        | Number of float
        | Identifier of string
        | PrimitiveProcedure of PrimitiveProcedure
        | Quote of P.StackCommand
        | Record of Map<string, Exp>
        | Array of Exp list
        // | Lambda
        | GeoExp of GeoExp

    and GeoExp =
        | LPoint of L.Point
        | LSegment of L.Segment
        | LLine of L.Line
        | LOperation of L.Operation
        | LPolygon of L.Polygon

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