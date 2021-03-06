namespace Tessa.Lex

open System
open Tessa.Util
open System.Text.RegularExpressions

module Lex = 
    open Util
    let always k _ = k

    type Pos = {row: int; col: int;}

    type LexError = LexError of Pos

    type PrimitiveProcToken = 
        | ArrayBuilder // []
        | RecordBuilder // {}
        | LinkPoints // +
        | Perpendicular // |- or T
        | Intersect // *
        | At // @
        | ApplyOp // %
        | Assign // =
        | RecordAccess // .
        | Snip // *-*
        | Draw // !
        | Fill
        | Stroke
        | Lambda
        | Is
        | DynamicBind
        | DynamicBindDraw
        | DynamicBindColor

    type Token = 
        | StackOp // :
        | EndStackOps // ;
        | BeginNestedExpression // (
        | EndNestedExpression // )
        | WhiteSpace // space, newline, comma?
        | Identifier of string // abc
        | DynamicIdentifier of string
        | Fraction of numer: int * denom: int // 11/4
        | String of string
        | PrimitiveProc of PrimitiveProcToken
        | QuotePrimitive // ' -- allows us pass + etc. into function as an argument without trigger stack
        // Primitive Procedures

    let identifierRegExp  = "\d*[a-zA-Z_\*]+[a-zA-Z\-\d_\*]*"

    let matchesToTokens = 
        [(":", always StackOp);
        ("\(", always BeginNestedExpression);
        ("\)", always EndNestedExpression);
        ("\[\]", always <| PrimitiveProc ArrayBuilder);
        ("\{\}", always <| PrimitiveProc RecordBuilder);
        ("\+", always <| PrimitiveProc LinkPoints);
        ("\|\-", always <| PrimitiveProc Perpendicular);
        ("->", always <| PrimitiveProc Lambda);
        ("<>", always <| PrimitiveProc DynamicBind);
        ("<!>", always <| PrimitiveProc DynamicBindDraw);
        ("<#>", always <| PrimitiveProc DynamicBindColor);
        ("\*", always <| PrimitiveProc Intersect);
        ("\@", always <| PrimitiveProc At);
        ("%", always <| PrimitiveProc ApplyOp);
        (";", always EndStackOps);
        ("=", always <| PrimitiveProc Assign);
        ("'", always QuotePrimitive);
        ("\"[a-zA-Z\d\s]+\"", fun (s: string) -> String (s.Substring(1, s.Length - 2)));
        ("\.", always <| PrimitiveProc RecordAccess);
        ("\!", always <| PrimitiveProc Draw);
        ("#=", always <| PrimitiveProc Stroke);
        ("#", always <| PrimitiveProc Fill);
        ("-/-", always <| PrimitiveProc Snip);
        ("is[^a-zA-Z]", always <| PrimitiveProc Is);
        ("\s", always WhiteSpace);
        ("--.*\n", always WhiteSpace);
        ("&" + identifierRegExp, DynamicIdentifier);
        (identifierRegExp, Identifier);
        ("\-*[\d]+/*[\d]*", fun s -> s.Split("/") |> (fun arr -> Fraction (int arr.[0], if arr.Length = 1 then 1 else int arr.[1])));]

    let advanceLex row col str = 
        if str = ""
        then Ok None
        else 
            let someMatch = 
                matchesToTokens 
                |> List.map (fun (regex, toToken) -> 
                    let m = Regex.Match(str, "^(" + regex + ")")
                    if isNull m || m.Value = "" || isNull m.Value then None else Some (m.Value, toToken m.Value))
                |> somes
                |> List.tryHead
            match someMatch with 
            | None -> Error <| LexError {row = row; col = col}

            | Some (m, tok) -> 
                let lastnewLine = m.LastIndexOf("\n")
                let (nextRow, nextCol) = if lastnewLine = -1 then (row, col + m.Length) else (row + 1, m.Length - lastnewLine)
                Ok <| Some ((tok, {row=row; col=col}), {row=nextRow;col=nextCol}, str.Remove(0, m.Length))

    let lex (topString:string) : Result<(Token * Pos) list, LexError>  =
        let rec go row col str = 
            match advanceLex row col str with 
            | Ok(None) -> Ok []
            | Ok(Some(result, nextRowCol, nextStr)) -> Result.map (fun g -> result :: g) <| go nextRowCol.row nextRowCol.col nextStr
            | Error e -> Error e

        go 0 0 topString |> Result.map (List.filter (function | (WhiteSpace,_) -> false | _ -> true))