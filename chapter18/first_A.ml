(* 問題 18.1 *)
(* 目的: person_t型のリストの中から最初のA型の人のレコードを返す *)
(* first_A: person_t list -> person_t option *)

type person_t = {
  name: string;
  bloodtype: string;
}

let rec first_A lst = match lst with
  [] -> None
  | { name = n; bloodtype = b } :: rest ->
    if b = "A" then Some ({ name = n; bloodtype = b })
    else first_A rest


let test0 = first_A [{name = "taro"; bloodtype = "A"}] = Some ({name = "taro"; bloodtype = "A"})
let test1 = first_A [{name = "jiro"; bloodtype = "A"}; { name = "Jack"; bloodtype = "B" }; {name = "taro"; bloodtype = "A"}] = Some ({name = "jiro"; bloodtype = "A"})
let test2 = first_A [] = None
let test3 = first_A [{name = "Jack"; bloodtype = "B"}] = None