(* 問題 10.5 *)
(* 目的: gakusei_t型のリストを受け取り、そのリスト中のフィールドseisekiが最も高いレコードを返す関数 *)
(* gakusei_max: gakusei_t list -> gakusei_t *)

type gakusei_t = {
  name: string;
  seiseki: int;
}

let rec gakusei_max lst = match lst with
  [] -> { name = "Fake"; seiseki = min_int }
  | first :: rest ->
    if (gakusei_max rest).seiseki < first.seiseki then
      first
    else
      gakusei_max rest

(* テスト *)
let test1 = gakusei_max [] = { name = "Fake"; seiseki = min_int }
let test2 = gakusei_max [{ name = "Taro"; seiseki = 100 }] = { name = "Taro"; seiseki = 100 }
let test3 = gakusei_max [{ name = "Taro"; seiseki = 100 }; { name = "Jiro"; seiseki = 110 }] = { name = "Jiro"; seiseki = 110 }
let test4 = gakusei_max [{ name = "Jiro"; seiseki = 110 }; { name = "Taro"; seiseki = 100 }] = { name = "Jiro"; seiseki = 110 }

(* 問題 10.6 *)

let rec gakusei_max_2 lst = match lst with
  [] -> { name = "Fake"; seiseki = min_int }
  | first :: rest ->
    let max_rest = gakusei_max_2 rest in
      if max_rest.seiseki < first.seiseki then
        first
      else
        max_rest

(* テスト *)
let test5 = gakusei_max_2 [] = { name = "Fake"; seiseki = min_int }
let test6 = gakusei_max_2 [{ name = "Taro"; seiseki = 100 }] = { name = "Taro"; seiseki = 100 }
let test7 = gakusei_max_2 [{ name = "Taro"; seiseki = 100 }; { name = "Jiro"; seiseki = 110 }] = { name = "Jiro"; seiseki = 110 }
let test8 = gakusei_max_2 [{ name = "Jiro"; seiseki = 110 }; { name = "Taro"; seiseki = 100 }] = { name = "Jiro"; seiseki = 110 }