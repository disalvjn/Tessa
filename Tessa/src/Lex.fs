namespace Tessa.Lex

open System
open Tessa.Util
open System.Text.RegularExpressions
open FSharpPlus

module Lex = 
    open Util
    let always k _ = k

    type Pos = {row: int; col: int;}

    type LexError = string * Pos

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
        | Lambda

    type Token = 
        | StackOp // :
        | EndStackOps // ;
        | BeginNestedExpression // (
        | EndNestedExpression // )
        | WhiteSpace // space, newline, comma?
        | Identifier of string // abc
        | Fraction of numer: int * denom: int // 11/4
        | PrimitiveProc of PrimitiveProcToken
        | QuotePrimitive // ' -- allows us pass + etc. into function as an argument without trigger stack
        // Primitive Procedures

    let matchesToTokens = 
        [(":", always StackOp);
        ("\(", always BeginNestedExpression);
        ("\)", always EndNestedExpression);
        ("\[\]", always <| PrimitiveProc ArrayBuilder);
        ("\{\}", always <| PrimitiveProc RecordBuilder);
        ("\+", always <| PrimitiveProc LinkPoints);
        ("\|\-", always <| PrimitiveProc Perpendicular);
        ("->", always <| PrimitiveProc Lambda);
        ("#", always <| PrimitiveProc Snip);
        ("\*", always <| PrimitiveProc Intersect);
        ("\@", always <| PrimitiveProc At);
        ("%", always <| PrimitiveProc ApplyOp);
        (";", always EndStackOps);
        ("=", always <| PrimitiveProc Assign);
        ("'", always QuotePrimitive);
        ("\.", always <| PrimitiveProc RecordAccess);
        ("\!", always <| PrimitiveProc Draw);
        ("\s", always WhiteSpace);
        ("[a-zA-Z{}!@#$%^&*-+~=\[\]]+[\d]*", Identifier);
        ("[\d]+/*[\d]*", fun s -> s.Split("/") |> (fun arr -> Fraction (int arr.[0], if arr.Length = 1 then 1 else int arr.[1])));]

    let advanceLex row col str = 
        if str = ""
        then Ok None
        else 
            let someMatch = 
                matchesToTokens 
                |> List.map (fun (regex, toToken) -> 
                    let m = Regex.Match(str, "^(" + regex + ")").Value
                    if m = "" then None else Some (m, toToken m))
                |> somes
                |> List.tryHead
            match someMatch with 
            | None -> Error <| LexError ("don't know what to do with starting character: " + str, {row = row; col = col})
            | Some (m, tok) -> 
                let lastnewLine = m.LastIndexOf("\n")
                let (nextRow, nextCol) = if lastnewLine = -1 then (row, col + m.Length) else (row + 1, m.Length - lastnewLine)
                Ok <| Some ((tok, {row=row; col=col}), {row=nextRow;col=nextCol}, str.Remove(0, m.Length))

    let lex (topString:string) : Result<(Token * Pos) list, LexError>  =
        let rec go row col str = 
            match advanceLex row col str with 
            | Ok(None) -> Ok []
            | Ok(Some(result, nextRowCol, nextStr)) -> map (fun g -> result :: g) <| go nextRowCol.row nextRowCol.col nextStr
            | Error e -> Error e

        go 0 0 topString |> map (List.filter (function | (WhiteSpace,_) -> false | _ -> true))