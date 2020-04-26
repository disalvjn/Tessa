namespace Tessa.EvalTests

open Xunit
open Tessa.Eval 
open Tessa.Lex 
open Tessa.Parse 

module EvalTests = 
    module Lex = Lex 
    module Parse = Parse
    module E = Eval

    let failAndPrint a = failwith (sprintf "%A" a)

    let fromResult = function
        | Error e -> failAndPrint e
        | Ok o -> o

    let fromNumber = function
        | E.Number n -> n
        | x -> failAndPrint x

    let fromSomeNumber = function 
        | Some(E.Number n) -> n
        | x -> failAndPrint x


    let lexAndParse s = 
        let lexed = fromResult <| Lex.lex s 
        let parsed = Parse.parse <| List.map fst lexed
        fromResult parsed

    let eval s = E.eval (lexAndParse s)


    [<Fact>]
    let ``Simple Addition`` () = 
        let result = eval "1 :plus 2 :plus 3;" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(6.0, asNum)

    [<Fact>]
    let ``Simple Addition Nested`` () = 
        let result = eval "1 :plus ((2 :plus 3) :plus 4);" |> fromResult
        let asNum = fromSomeNumber result.currentContext.ret
        Assert.Equal(10.0, asNum)