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

   // type Either<'l, 'r> = 
   //    | Left of 'l
   //    | Right of 'r
   //    static member Bind (x, f) = 
   //       match x with
   //       | Left l -> Left l
   //       | Right r -> f r

   let inline (>>=) (x:'ma) (f:'a -> 'mb) = (^ma: (static member Bind: 'ma * ('a -> 'mb) -> 'mb) (x, f))

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

   type Result<'a, 'b> with
      static member Bind (x, f) = 
         match x with
         | Error l -> Error l
         | Ok r -> f r
   
   let result = ResultBuilder ()


      