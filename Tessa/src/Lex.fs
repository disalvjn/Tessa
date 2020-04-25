namespace Tessa.Lex

open System
open Tessa.Util
open System.Text.RegularExpressions

module Lex = 
    open Util
    let always k _ = k

    type Pos = {row: int; col: int;}
    type Token = 
        | StackOp // :
        | EndStackOps // ;
        | BeginNestedExpression // (
        | EndNestedExpression // )
        | WhiteSpace // space, newline, comma?
        | Identifier of string // abc
        | Fraction of numer: int * denom: int // 11/4
        // Primitive Procedures
        | ArrayBuilder // []
        | RecordBuilder // {}
        | LinkPoints // +
        | Perpendicular // |- or T
        | Intersect // *
        | At // @
        | ApplyOp // %
        | Assign // =
        | QuotePrimitive // ' -- allows us pass + etc. into function as an argument without trigger stack
        | RecordAccess // .
        | Snip // *-*
        | Draw // !
        | Lambda
        | CellBuilder // <#>

    let matchesToTokens = 
        [(":", always StackOp);
        ("\(", always BeginNestedExpression);
        ("\)", always EndNestedExpression);
        ("\[\]", always ArrayBuilder);
        ("\{\}", always RecordBuilder);
        ("\+", always LinkPoints);
        ("\|\-", always Perpendicular);
        ("->", always Lambda);
        ("\*-\*", always Snip);
        ("\*", always Intersect);
        ("\@", always At);
        ("%", always ApplyOp);
        (";", always EndStackOps);
        ("=", always Assign);
        ("'", always QuotePrimitive);
        ("\.", always RecordAccess);
        ("\!", always Draw);
        ("<#>", always CellBuilder);
        ("\s", always WhiteSpace);
        ("[a-zA-Z{}!@#$%^&*-+~=\[\]]+[\d]*", Identifier);
        ("[\d]+/*[\d]+", fun s -> s.Split("/") |> (fun arr -> Fraction (int arr.[0], int arr.[1])));]

    let advanceLex row col str = 
        if str = ""
        then None
        else 
            let someMatch = 
                matchesToTokens 
                |> List.map (fun (regex, toToken) -> 
                    let m = Regex.Match(str, "^(" + regex + ")").Value
                    if m = "" then None else Some (m, toToken m))
                |> somes
                |> List.tryHead
            match someMatch with 
            | None -> failwith <| "don't know what to do with starting character: " + str
            | Some (m, tok) -> 
                let lastnewLine = m.LastIndexOf("\n")
                let (nextRow, nextCol) = if lastnewLine = -1 then (row, col + m.Length) else (row + 1, m.Length - lastnewLine)
                Some <| ((tok, {row=row; col=col}), {row=nextRow;col=nextCol}, str.Remove(0, m.Length))

    let lex (topString:string) : (Token * Pos) list  =
        let rec go row col str = 
            match advanceLex row col str with 
            | None -> []
            | Some(result, nextRowCol, nextStr) -> result :: go nextRowCol.row nextRowCol.col nextStr

        go 0 0 topString |> List.filter (function | (WhiteSpace,_) -> false | _ -> true)