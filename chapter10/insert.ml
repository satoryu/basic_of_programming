(* 問題 10.1 *)
(* 目的: 与えられた整数のリストに引数として与えられた整数を挿入する。前から順に見ていき、昇順となる位置に挿入する。*)
(* insert: int list -> int -> int list *)

let rec insert lst n = match lst with
  [] -> [n]
  | first :: rest -> if first < n then first :: (insert rest n) else n :: lst

(* テスト *)
let test1 = insert [] 1 = [1]
let test2 = insert [1] 2 = [1; 2]
let test3 = insert [2] 1 = [1; 2]
let test4 = insert [1; 3] 2 = [1; 2; 3]
let test5 = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8]


(* 問題 10.2 *)
(* 目的: 整数のリストを受け取ったら、それを昇順に整列したリストを返す。 *)
(* ins_sort: int list -> int list *)

let rec ins_sort lst = match lst with
    [] -> []
    | first :: rest -> insert (ins_sort rest) first

(* テスト *)
let test6 = ins_sort [] = []
let test7 = ins_sort [1] = [1]
let test8 = ins_sort [1; 2] = [1; 2]
let test9 = ins_sort [2; 1] = [1; 2]
let test10 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8]

(* 問題 10.3 *)
(* 目的: 与えられたgakusei_t 型のリストを、tensuuフィールドの順に整列したリストを返す *)
(* gakusei_sort: gakusei_t list -> gakusei_t list *)

type gakusei_t = {
  seiseki : int; (* 成績 *)
}

let rec gakusei_insert lst g = match lst with
  [] -> [g]
  | first :: rest -> if first.seiseki < g.seiseki then first :: (insert rest g) else g :: lst

let rec gakusei_sort lst = match lst with
  [] -> []
  | first :: rest -> gakusei_insert (gakusei_sort rest) first

let list01 = [
  { seiseki = 30 };
  { seiseki = 90 };
  { seiseki = 40 }
]
let test11 = gakusei_sort list01 = [{ seiseki = 30 }; { seiseki = 40 }; { seiseki = 90 }]
