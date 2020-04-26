namespace Tessa.Util

open System
open FSharp.Collections
open FSharpPlus

module Util =
   let rec okays list = 
     match list with
     | [] -> []
     | Ok o :: rest -> o :: okays rest
     | Error _ :: rest -> okays rest

   let rec errors list = 
      match list with 
      | [] -> []
      | Ok o :: rest -> errors rest 
      | Error e :: rest -> e :: errors rest

   let rec somes list =
      match list with
      | [] -> []
      | Some s :: rest -> s :: somes rest 
      | None :: rest -> somes rest

   let cons x y = x :: y

   let tryCons item list = 
      match item with 
      | None -> list 
      | Some s -> s :: list

// http://matthewmanela.com/blog/functional-stateful-program-in-f/

module Result = 
   let bimap ok error = function
      | Ok o -> Ok <| ok o
      | Error e -> Error <| error e
      
   let bind2 result1 result2 f = 
      Result.bind (fun r1 ->
         Result.bind (fun r2 -> f r1 r2) result2
      ) result1
   
   let cata ok error = function 
      | Ok o -> ok o
      | Error e -> error e

   let fromOk errorDefault = function
      | Ok o -> o 
      | Error _ -> errorDefault

   let fromError okDefault = function 
      | Ok _ -> okDefault
      | Error e -> e

   let mapError f x = bimap id f x

   let sequence xs = List.fold (fun acc x -> bind2 acc x (fun acc' x' -> Ok <| x' :: acc')) (Ok []) xs

   type ResultBuilder() =
      member b.Return(x) = Ok x
      member b.ReturnFrom(x) = x
      member b.Error e = Error e
      member b.Bind (x, f) = 
         match x with 
         | Ok o -> f o
         | Error e -> Error e

   let result = ResultBuilder ()

   let isOkay = function 
      | Ok _ -> true 
      | Error _ -> false


      