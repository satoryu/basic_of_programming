(* 問題 14.1 *)
(* 目的 与えられた整数のリストから偶数のものを抜き出し、それらをリストとして返す *)
(* even: int lst -> int lst *)

let rec filter p lst = match lst with
  [] -> []
  | first :: rest -> if p first then first :: (filter p rest)
                                else filter p rest

(* is_even: int -> boolean *)
let is_even n = (n mod 2) = 0

let even lst = filter is_even lst

let test0 = even [] = []
let test1 = even [0;] = [0]
let test2 = even [1;] = []
let test3 = even [2;] = [2]
let test4 = even [3; 4; 5; 2] = [4; 2]

(* 問題 14.2 *)
(* 目的 gakusei_t のリストが与えられたとき、その中で成績がAのものの数を返す *)
(* count_A: lst gakusei_t -> int *)

type gakusei_t = {
  seiseki : string
}

let rec length lst = match lst with
  [] -> 0
  | first :: rest -> 1 + length rest

(* is_A: gakusei_t -> boolean*)
let is_A g = match g with
  { seiseki = s } -> s = "A"

let count_A lst = length (filter is_A lst)

let test5 = count_A [] = 0
let test6 = count_A [{seiseki = "B"}] = 0
let test7 = count_A [{seiseki = "A"}] = 1
let test8 = count_A [{seiseki = "B"}; {seiseki = "A"}] = 1
let test9 = count_A [{seiseki = "B"}; {seiseki = "A"}; {seiseki = "A"}] = 2
