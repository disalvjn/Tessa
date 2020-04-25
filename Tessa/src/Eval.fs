namespace Tessa.Eval

open Tessa.Language
open Tessa.Solve
open Tessa.Parse
open Tessa.Util

// todo: make sure Parse and Solve both return Result
module Eval = 
    module P = Parse
    module L = Language
    module S = Solve

    type Operation =
        | Primitive of P.PrimitiveProcedure
        | Custom of string

    type Exp =
        | Number of float
        | Function 
        | Identifier
        | Record
        | LanguageExp of LanguageExp
        // Language Unsolved
    
    and LanguageExp = 
        | LPoint of L.Point
        | LSegment of L.Segment
        | LLine of L.Line
        | LOperation of L.Operation
        | LPolygon of L.Polygon

    and SolveExp = 
        | SPoint of S.Point
        | SSegment of S.Segment
        | SLine of S.Line

    type StackExecutionContext = {
        currentOp: Operation option;
        stack: P.Word list;
        environment: Map<string, Exp>;
        // continuation implicitly stored in list
    }

    type ExecutionContext = {
        // stackContext[i] has continuation stackContext[i + 1]
        stackContexts: StackExecutionContext list;
        globalEnvironment: Map<string, Exp>;
        subExpressions: Map<Exp, Exp>;
        errors: string list;
        // have a Solve function to seemlessly evaluate 
    }



    // let primitiveLookup : Map<string, (Exp list -> Exp)> =
    //     [("add", )]
