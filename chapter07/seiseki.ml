(* 問題 7.2 *)

let seiseki (name, score) = name ^ "さんの評価は" ^ string_of_int score ^ "です"

let test1 = seiseki ("Taro", 100) = "Taroさんの評価は100です"

