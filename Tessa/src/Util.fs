namespace Tessa.Util

open System
open FSharp.Collections

module Util =
   let rec okays list = 
     match list with
     | [] -> []
     | Ok o :: rest -> o :: okays rest
     | Error _ :: rest -> okays rest

   let rec okay = function
      | Ok o -> o
      | Error e -> failwith e

   let rec somes list =
      match list with
      | [] -> []
      | Some s :: rest -> s :: somes rest 
      | None :: rest -> somes rest

   let cons x y = x :: y

// http://matthewmanela.com/blog/functional-stateful-program-in-f/
module State = 
   type StateData<'st, 'a> =
      | Ok of 'a * 'st
      | Error of string 

   type State<'st, 'a> = 'st -> StateData<'st, 'a>

   and StateBuilder() = 
      member b.Return(x) : State<'st, 'a> = fun s -> Ok(x, s)
      member b.ReturnFrom(x) = x
      member b.Error msg : State<'st, 'a> = fun _ -> Error msg
      member b.Bind(f: 'a -> State<'st, 'b>, x: State<'st, 'a>) : State<'st, 'b>= 
       fun state ->
         let result = x state in
         match result with
            | Ok (value, state2) -> f value state2
            | Error e -> Error e
      member b.Get() = fun state -> Ok (state, state)
      member b.Put s = fun state -> Ok((), s)

module Result = 
   let bimap ok error = function
      | Ok o -> Ok <| ok o
      | Error e -> Error <| error e