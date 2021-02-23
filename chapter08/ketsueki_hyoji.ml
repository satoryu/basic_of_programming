(* 問題8.4 *)

type person_t = {
  name : string; (* 名前 *)
  bloodtype : string (* 血液型 *)
}

(* 目的: person_t型で与えられた情報を元に、その人の名前と血液型を表す文字列を返す *)
(* ketueki_hyoji : person_t -> string *)
let ketueki_hyoji person = match person with
  {name = n; bloodtype = b} -> n ^ "さんの血液型は" ^ b ^ "型です"

let test1 = ketueki_hyoji { name = "Tatsuya"; bloodtype = "AB" } = "Tatsuyaさんの血液型はAB型です"
