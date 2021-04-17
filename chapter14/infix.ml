(* 問題14.14 *)

let rec fold_right f lst init = match lst with
  [] -> init
  | first :: rest -> f first (fold_right f rest init)

let concat lst = fold_right (^) lst ""

let test0 = concat ["BABY"; "METAL"; " "; "DEATH"] = "BABYMETAL DEATH"

(* 問題14.15 *)
(* 目的: 1から受け取った自然数までの合計数を求める *)
(* one_to_n: int -> int *)

let rec enumerate n =
  if n = 0 then [] else n :: enumerate(n - 1)

let one_to_n n = fold_right (+) (enumerate n) 0

let test0 = one_to_n 0 = 0
let test1 = one_to_n 1 = 1
let test2 = one_to_n 2 = 3
let test3 = one_to_n 3 = 6

(* 問題 14.16 *)
(* fac: int -> int *)

let fac n = fold_right ( * ) (enumerate n) 1

let test5 = fac 1 = 1
let test6 = fac 2 = 2
let test7 = fac 3 = 6
let test8 = fac 4 = 24