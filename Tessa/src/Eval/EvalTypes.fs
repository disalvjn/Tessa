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
        | MakeLambda
        | Is
        | Eval
        | DynamicBind
        | DynamicBindDraw

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
        | WrongArgumentsEval of Exp list
        | WrongArgumentsDynamicBind of Exp list
        | FunctionCallArgumentMismatch of Exp list * string list
        | LambdaArgumentNotSymbol of EvalError
        | LambdaBodyNotExpression of Exp list

    and Exp =
        | Number of float
        | Identifier of string
        | PrimitiveProcedure of PrimitiveProcedure
        | Quote of P.StackCommand
        | Record of Map<string, Exp>
        | Array of Exp list
        | LambdaExp of Lambda
        | GeoExp of GeoExp
        // An operation by itself does nothing, it's solved in the context of a point.
        // But, they're still first class citizens. One solution is GeoExp having two branches,
        // one for shapes and one for the operation.
        | LOperation of L.Operation

    and Lambda = Lambda of Environment * string list * P.StackCommand


    and DrawMap = Map<CellName, GeoExp list>
    and Environment = Map<string, Exp>
    and DynamicEnvironment = Map<string, Exp>

    and GeoExp =
        | LPoint of L.Point
        | LSegment of L.Segment
        | LLine of L.Line
        | LCell of L.Cell

    let toNumber exp = 
        match exp with 
        | Number n -> Ok n 
        | other -> Error other 

    type Operation =
        | Primitive of PrimitiveProcedure
        | LambdaOp of Lambda
        // | Function of Lambda
        // | Fun

    type OperationState = 
        | Empty 
        | EmptyAcceptNext
        | Op of Operation


    type Runtime = {
        drawMap: DrawMap;
        environment: Environment;
        dynamicEnvironment: DynamicEnvironment;
    }

    type EvaluatorMessage =
        | AugmentEnvironment of Map<string, Exp>
        | DrawGeo of string * GeoExp

    type EvalResult = {
        value: Exp option;
        runtime: Runtime;
        error: EvalError option;
    }
