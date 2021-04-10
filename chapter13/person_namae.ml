(* 問題 13.2 *)
(* 目的 受け取ったperson_t型のリストからその中に出てくる人の名前のリストを返す *)
(* person_namae: person_t lst -> string lst *)

type person_t = {
  name : string;
}

let rec map f lst = match lst with
  [] -> []
  | first::rest -> f first :: map f rest

let name_of_person p = match p with
  { name = n } -> n

let rec person_name lst = map name_of_person lst

let people = [
  { name = "Kokona Nonaka" };
  { name = "Sana Shiratori" };
  { name = "Neo Sato"};
]

let t = person_name people
let test0 = person_name people = ["Kokona Nonaka"; "Sana Shiratori"; "Neo Sato" ]
