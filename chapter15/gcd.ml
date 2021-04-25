(* 問題 15.2 *)
(* 目的: 与えられた2つの整数の最大公約数を求める *)
(* gcd: int -> int -> int *)

let rec gcd m n =
  if n = 0 then m
  else gcd n (m mod n)

let test0 = gcd 2 1 = 1
let test1 = gcd 3 0 = 3
let test2 = gcd 4 2 = 2
let test3 = gcd 9 6 = 3

