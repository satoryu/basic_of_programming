let rec fold_right f lst init = match lst with
  [] -> init
  | first :: rest -> f first (fold_right f rest init)

(* 問題 14.3 *)
(* 目的: 与えられた文字列のリストに含まれる文字列要素をその順番に連結した文字列を返す *)
(* concat: string lst -> string *)

let concat_two_string a b = a ^ b

let concat lst = fold_right concat_two_string lst ""

let test0 = concat [] = ""
let test1 = concat [""] = ""
let test2 = concat ["A"] = "A"
let test3 = concat ["A"; "B"] = "AB"
let test4 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"

(* 問題 14.4 *)
(* 目的: 与えられたgakusei_t型のリストを受け取り、その中の得点の合計を返す *)
(* gakusei_sum: gakusei_t lst -> int *)
type gakusei_t = {
  score : int
}

let rec map f lst = match lst with
  [] -> []
  | first :: rest -> f first :: (map f rest)

let gakusei_score g = match g with
  { score = s } -> s

let add x y = x + y

let gakusei_sum lst = fold_right add (map gakusei_score lst) 0

let test5 = gakusei_sum [] = 0
let test6 = gakusei_sum [{score = 10}] = 10
let test7 = gakusei_sum [{score = 10}; {score = 100}] = 110
