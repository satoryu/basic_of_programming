(* 問題13.3 *)

(* (1) *)
(* 'a -> 'a *)
let f1 x = x

(* (2) *)
(* 'a -> 'b -> 'a *)
let f2 x y = x

(* (3) *)
(* 'a -> 'b -> 'b *)
let f3 x y = y

(* (4) *)
(* 'a -> ('a -> 'b) -> 'b *)
let f4 x g = g x

(* (5) *)
(* ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let f5 f g =
  let h x = g (f x)
  in h

(* 問題 13.4 *)
(* compose ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
let compose f g =
  let h x = f (g x) in h

let time2 x = 2 * x
let add3 x = 3 + x

let test = (compose time2 add3) 4 = 14

(* 問題 13.5 *)
(* twice: ('a -> 'a) -> 'a -> 'a *)
let twice f = let g x = f (f x) in g
let g = twice twice
