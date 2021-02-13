(* 問題 4.6 *)
(* 目的: 鶴の数を与えると、足の本数を返す関数を作成する *)
(* 鶴の数を整数として渡し、鶴の足の数を整数として返す *)

let tsuru_no_ashi n = n * 2

(* テスト *)

let test1 = tsuru_no_ashi 0 = 0
let test2 = tsuru_no_ashi 1 = 2
let test3 = tsuru_no_ashi 10 = 20

(* 目的: 亀の数を与えると、亀の足の本数を計算する *)

let kame_no_ashi n = n * 4

(* テスト *)
let test4 = kame_no_ashi 0 = 0
let test5 = kame_no_ashi 1 = 4
let test6 = kame_no_ashi 10 = 40
