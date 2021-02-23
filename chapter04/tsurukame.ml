(* 問題 4.8 *)
(* 目的: 鶴と亀の合計の引数と足の合計を与えると、そのうちの鶴の数を計算する *)

let tsurukame n m = n - (m - 2 * n) / 2

(* テスト *)

let test1 = tsurukame 0 0 = 0
let test2 = tsurukame 1 2 = 1
let test3 = tsurukame 1 4 = 0
let test4 = tsurukame 2 10 = -1
let test5 = tsurukame 8 26 = 3
