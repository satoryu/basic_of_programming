(* 問題 4.7 *)
(* 目的: 引数として鶴の数、亀の数を与えると、それらの合計の足の本数を計算する *)

#use "tsuru_no_ashi.ml"

let tsurukame_no_ashi n m = tsuru_no_ashi n + kame_no_ashi m

(* テスト *)
let test1 = tsurukame_no_ashi 0 0 = 0
let test2 = tsurukame_no_ashi 0 1 = 4
let test3 = tsurukame_no_ashi 1 0 = 2
let test4 = tsurukame_no_ashi 1 1 = 6
let test5 = tsurukame_no_ashi 2 3 = 16
