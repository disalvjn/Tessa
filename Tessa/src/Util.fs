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

module State =

   type State<'a, 's> = 's -> 'a * 's

   let bind (x: State<'a, 's>) (f: 'a -> State<'b, 's>) : State<'b, 's> =
      fun state ->
         let (a, state1) = x state
         f a state1

   let runState (computation: State<'a, 's>) (state: 's) = computation state |> fst

   let ret a = fun state -> a
