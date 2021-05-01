(* 問題16.1 *)
(* 目的: 受け取った整数のリストの、それまでの数の合計からなるリストを求める *)
(* sum_lst: int lst -> int lst *)

let sum_lst lst =
  let rec hojo lst total0 = match lst with
  [] -> []
  | first :: rest -> let total = first + total0 in total :: (hojo rest total)
  in hojo lst 0

let test0 = sum_lst [] = []
let test1 = sum_lst [1] = [1]
let test2 = sum_lst [1; 3] = [1; 4]
let test3 = sum_lst [3; 2; 1; 4] = [3; 5; 6; 10]
