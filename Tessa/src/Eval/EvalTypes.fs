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
        | Assign
        | RecordBuilder 
        | RecordAccess
        | ArrayBuilder 
        | Lambda

        | LinkPoints 
        | Perpendicular 
        | Intersect 
        | At 
        | ApplyOp 
        | Snip
        | Draw
        // has optional assignment semantics also! more convenient.
        | CellBuild

    // todo: Need to pipe Lex pos into Parse so I can add positions here
    type EvalError =
        | UndefinedVariable of var: string * message: string
        | AddingNonNumbers of Exp list
        | ApplyingNonFunction of Exp
        | UnbalancedParenExtraClose
        | AssignError
        | RecordBuildingError
        | RecordAccessError of field: string * record: Exp option

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