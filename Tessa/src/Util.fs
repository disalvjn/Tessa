namespace Tessa.Util

open System
open FSharp.Collections

// State monad taken from FSharpPlus (Apache 2.0 license)
// https://github.com/fsprojects/FSharpPlus/blob/master/src/FSharpPlus/Data/State.fs#L13-13
// I'd use FSharpPlus, but it doesn't play well with Fable. In fact, I started the project using it,
// but had to remove it once I started testing in the browser.

type State<'s,'t> = State of ('s->('t * 's))

module State =

   let run (State x) = x 

   let map   f (State m) = State (fun s -> let (a: 'T, s') = m s in (f a, s'))
   let bind  f (State m) = State (fun s -> let (a: 'T, s') = m s in run (f a) s') 
   let apply (State f) (State x) = State (fun s -> let (f', s1) = f s in let (x': 'T, s2) = x s1 in (f' x', s2)) : State<'S,'U>

   let eval (State sa) (s: 's)         = fst (sa s) 
   let exec (State sa: State<'S,'A>) s = snd (sa s) 

    /// Return the state from the internals of the monad.
   let get = State (fun s -> (s, s))

    /// Get a value which depends on the current state.
   let gets f = State (fun s -> (f s, s))

    /// Replace the state inside the monad.
   let put x = State (fun _ -> ((), x))

    /// Modify the state inside the monad by applying a function.
   let modify f = State (fun s -> ((), f s))

   let result a = State (fun s -> (a, s))

   let rec sequence (coll: State<'s, 'a> list) : State<'s, 'a list> = 
      match coll with 
      | [] -> result []
      | x :: xs -> sequence xs |> bind (fun many -> x |> bind (fun single -> result <| single :: many))

   type StateBuilder() =
      member b.Return(a) = State (fun s -> (a, s))
      member b.ReturnFrom(x) = x
      member b.Bind (x, f) = bind f x


module Result = 
   let toOption = function 
      | Ok o -> Some o 
      | _ -> None 

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

   let sequence xs = List.fold (fun acc x -> bind2 acc x (fun acc' x' -> Ok <| x' :: acc')) (Ok []) xs |> Result.map List.rev

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
      
module Util =
   module R = Result 
   let result = R.result

   module S = State
   let state = S.StateBuilder ()

   let flip f x y = f y x 
   let tuple2 a b = (a, b)

   let complement f x =  not (f x)

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

   let failAndPrint a = failwith (sprintf "%A" a)

   let listToMap lst = List.fold (fun m (k, v) -> Map.add k v m) Map.empty lst

   let firstOption x y = 
      match (x, y) with
      | (Some(_), _) -> x
      | _ -> y

   let curry f x y = f (x, y)
   let uncurry f (x, y) = f x y

module Option = 
   let cata some none option = 
      match option with 
      | Some s -> some s 
      | None -> none

module Map = 
   let filterSome pred m = Map.fold (fun m k v -> Option.cata (fun s -> Map.add k s m) m (pred v)) Map.empty m
   let mapList f m = Map.fold (fun l k v -> (f k v) :: l) [] m
   let mapListMany f m = Map.fold (fun l k v -> (f k v) @ l) [] m

   let unionWith f m n = Map.fold (fun m k v -> match Map.tryFind k m with | Some ov -> Map.add k (f ov v) m | None -> Map.add k v m) m n

   let union m n = unionWith (fun v1 v2 -> v1) m n

   let values m = Map.toList m |> List.map snd

module List = 
   let rec cartesianProduct list1 list2 =  
      match list1 with 
      | [] -> []
      | x :: ys -> List.map (fun a -> (x, a)) list2 @ cartesianProduct ys list2
   
   let unpack list = List.collect (fun (x, y) -> [x; y]) list