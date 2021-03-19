(* 問題 9.4 *)
(* 目的: 受け取った整数のリストの長さを返す *)
(* length : int list -> int *)

let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest

let test01 = length [] = 0
let test02 = length [10] = 1
let test03 = length [2; 1; 6; 4; 7] = 5

(* 問題 9.5 *)
(* 目的: 受け取った整数のリストのうち、偶数の要素のみを含むリストを返す *)
(* even : int list -> int list *)

let rec even lst = match lst with
    [] -> []
  | first :: rest -> if (first mod 2) = 0 then first :: even rest
                                          else even rest

let test04 = even [] = []
let test05 = even [1] = []
let test06 = even [4] = [4]
let test07 = even [2; 1; 8; 11; 6] = [2; 8; 6]

(* 問題 9.6 *)
(* 目的: 与えられた文字列のリストに含まれる文字列要素をその順番に連結した文字列を返す *)
(* concat : string list -> string *)

let rec concat lst = match lst with
    [] -> ""
  | first :: rest -> first ^ concat rest

let test08 = concat [""] = ""
let test09 = concat [] = ""
let test10 = concat ["a"; "b"] = "ab"
let test11 = concat ["c"; ""; "d"] = "cd"
