(* 問題 13.1 *)
(* 目的: 受け取ったperson_t型のリストの中から指定された血液型の人の数を返す *)
(* count_ketsueki: person_t list -> int *)
type person_t = {
  height : float;
  weight : float;
  bloodtype : string;
}

let rec count_ketsueki lst ketsueki0 = match lst with
  [] -> 0
  | { height = h; weight = w; bloodtype = b} :: rest ->
    if b = ketsueki0 then 1 + count_ketsueki rest ketsueki0
    else count_ketsueki rest ketsueki0


let people = [
  { height = 169.0; weight = 64.8; bloodtype = "AB"};
  { height = 142.0; weight = 30.3; bloodtype = "A"};
  { height = 139.0; weight = 48.9; bloodtype = "B"};
  { height = 167.0; weight = 55.5; bloodtype = "B"}
]

let test0 = count_ketsueki people "AB" = 1
let test1 = count_ketsueki people "A" = 1
let test2 = count_ketsueki people "B" = 2
let test3 = count_ketsueki people "O" = 0
