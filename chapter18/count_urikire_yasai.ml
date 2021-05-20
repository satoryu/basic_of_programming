(* 問題 18.2 *)
(* 目的: 野菜のリストと八百屋のリストを受け取ったら、野菜のリストのうち八百屋においてない野菜の数を数える *)
(* count_urikire_yasai: string list -> (string * int) list -> int *)

let rec price item yaoya_list = match yaoya_list with
  [] -> None
  | (yasai, nedan) :: rest  ->
    if item = yasai then Some (nedan)
    else price item rest

let rec count_urikire_yasai yasai_list yaoya_list = match yasai_list with
    [] -> 0
    | first :: rest -> match price first yaoya_list with
      None -> count_urikire_yasai rest yaoya_list
      | Some (p) -> 1 + count_urikire_yasai rest yaoya_list

let yaoya_list = [
  ("トマト", 300); ("たまねぎ", 200);
  ("にんじん", 150); ("ほうれん草", 200)
]

let test0 = count_urikire_yasai [] yaoya_list = 0
let test1 = count_urikire_yasai ["トマト"] yaoya_list = 1
let test2 = count_urikire_yasai ["ブロッコリー"] yaoya_list = 0
let test3 = count_urikire_yasai ["トマト"; "ほうれん草"; "プリン"] yaoya_list = 2
let test3 = count_urikire_yasai ["トマト"; "ほうれん草"; "トマト"] yaoya_list = 3
