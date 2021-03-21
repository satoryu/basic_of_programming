(* 問題 10.7 *)
(* 目的: 与えられたperson_t 型のリストの中の各血液型ごとに何人いるかを組にして返す *)
(* ketsueki_shukei: person_t list -> int * int * int * in *)

type person_t = {
  name: string;
  bloodtype: string;
}

let rec ketsueki_shukei lst = match lst with
  [] -> (0, 0, 0, 0)
  | { bloodtype = bt } :: rest ->
    let (a, b, o, ab) = ketsueki_shukei rest in
      if bt = "A" then (a + 1, b, o, ab)
      else if bt = "B" then (a, b + 1, o, ab)
      else if bt = "O" then (a, b, o + 1, ab)
      else (a, b, o, ab + 1)

let people = [
  { name = "SU-METAL"; bloodtype = "B" };
  { name = "MOAMETAL"; bloodtype = "A" };
  { name = "YUIMETAL"; bloodtype = "O" }
]

let test1 = ketsueki_shukei people = (1, 1, 1, 0)

(* 問題 10.8 *)
(* 目的: person_t型のリストが与えられたときに、その中で最も人数が多い血液型を返す *)
(* saita_ketsueki: person_t list -> string *)

type shukei_result_t = {
  bloodtype: string;
  count: int;
}

(* max_blood_type shukei_result_t list -> shukei_result_t *)
let rec max_blood_type lst = match lst with
  [] -> { bloodtype = "Fake"; count = min_int }
  | first :: rest ->
    let max_rest = max_blood_type rest in
      if max_rest.count > first.count then max_rest else first

let rec saita_ketsueki lst =
  let (a, b, o, ab) = ketsueki_shukei lst in
    let bt = max_blood_type [{ bloodtype = "A"; count = a}; { bloodtype = "B"; count = b }; { bloodtype = "O"; count = o }; { bloodtype = "AB"; count = ab }] in
      bt.bloodtype

let people = [
  { name = "SU-METAL"; bloodtype = "B" };
  { name = "MOAMETAL"; bloodtype = "A" };
  { name = "YUIMETAL"; bloodtype = "O" };
  { name = "KOBAMETAL"; bloodtype = "B" }
]

let test2 = saita_ketsueki people = "B"
