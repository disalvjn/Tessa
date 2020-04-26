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

   let rec okay = function
      | Ok o -> o
      | Error e -> failwith e

   let rec somes list =
      match list with
      | [] -> []
      | Some s :: rest -> s :: somes rest 
      | None :: rest -> somes rest
   let cons x y = x :: y

   // let bind2 x y f = monad {
   //    let! xb = x
   //    let! yb = y
   //    return! f x y}


// http://matthewmanela.com/blog/functional-stateful-program-in-f/
module State = 
   type StateData<'st, 'a, 'e> =
      | Ok of 'a * 'st
      | Error of 'e

   type State<'st, 'a, 'e> = RunState of ('st -> StateData<'st, 'a, 'e>)

   and StateBuilder() = 
      member b.Return(x) : State<'st, 'a, 'e> = RunState (fun s -> Ok(x, s))
      member b.ReturnFrom(x) = x
      member b.Error msg  : State<'st, 'a, 'e> = RunState(fun _ -> Error msg)
      member b.Bind(RunState(s), f) = 
       RunState(fun state ->
         let result = s state in
         match result with
            | Ok (value, state2) -> match f value with 
               | RunState(fs) -> fs state2
            | Error e -> Error e)
      member b.Get () = RunState(fun state -> Ok (state, state))
      member b.Put s = RunState(fun state -> Ok((), s))

      member b.Update f = 
         b {
            let! s = b.Get ()
            do! b.Put (f s)
            return! b.Get ()
         }

      member b.Lift (result: Result<'a, 'e>) : State<'st, 'a, 'e> = 
         RunState(fun state -> 
            match result with
               | Result.Ok o -> Ok(o, state)
               | Result.Error e -> Error e)

      member b.Bind2 (RunState(x)) (RunState(y)) f = 
         RunState(fun state ->
            match x state with
               | Ok (a, state2) -> match y state2 with 
                  | Ok (b, state3) -> let (RunState(fs)) = f a b in fs state3
                  | Error e -> Error e
               | Error e -> Error e)

      
   let state = StateBuilder()





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


      