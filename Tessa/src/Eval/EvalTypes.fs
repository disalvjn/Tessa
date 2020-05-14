namespace Tessa.Eval.Types

open Tessa.Language
open Tessa.Solve.Types
open Tessa.Parse

// todo: make sure Parse and Solve both return Result
module EvalTypes = 
    module P = Parse
    module L = Language
    module S = SolveTypes

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
        | NotACell of Exp
        | NotAnOperation of Exp
        | WrongArgumentsToAt of Exp list
        | WrongArgumentsToRotation of Exp list
        | WrongArgumentsToApplyOp of Exp list
        | WrongArgumentsToSnip of Exp list
        | WrongArgumentsToDraw of Exp list
        | WrongArgumentsEval of Exp list
        | WrongArgumentsDynamicBind of Exp list
        | WrongArgumentsDynamicBindDraw of Exp list
        | FunctionCallArgumentMismatch of Exp list * string list
        | LambdaArgumentNotSymbol of EvalError
        | LambdaBodyNotExpression of Exp list
        | DrawDynamicVarImproperlyBound of Exp
        | DrawDynamicVarUnbound

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


    and Environment = Map<string, Exp>
    and DynamicEnvironment = Map<string, Exp>

    and GeoExp =
        | LPoint of L.Point
        | LSegment of L.Segment
        | LLine of L.Line
        | LCell of L.Cell

    let asPoint x =
        match x with 
        | GeoExp (LPoint p) -> Ok p 
        | _ -> Error <| NotAPoint x

    let asCell x =
        match x with
        | GeoExp (LCell c) -> Ok c 
        | _ -> Error <| NotACell x

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
        environment: Environment;
        dynamicEnvironment: DynamicEnvironment;
        labels: Map<string, L.Point>;
    }

    type EvaluatorMessage =
        | AugmentEnvironment of Map<string, Exp>
        | AugmentDynamicEnvironment of Map<string, Exp>
        | AugmentEnvAndLabels of Map<string, L.Point>

    type EvalResult = {
        value: Exp option;
        runtime: Runtime;
        error: EvalError option;
    }
