(* 問題 16.2 *)
(* 目的: 関数fと初期値init、そしてリストlstを受け取ったら、initから始めてリストlstの左から順にfを施しこむ *)
(* fold_left: (a' -> a') -> a' lst -> a' -> a' *)

let fold_left f lst init =
  let rec fold f lst a0 = match lst with
    [] -> a0
    | first :: rest -> let a = f a0 first in fold f rest a
  in fold f lst init

let test0 = fold_left (+) [1; 2; 3] 0 = 6
let test1 = fold_left ( * ) [1; 2; 3;] 10 = 60
let test2 = fold_left (^) ["BABY"; "METAL"] "BLACK" = "BLACKBABYMETAL"
