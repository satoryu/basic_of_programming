(* 問題 9.7 *)
(* 目的: 与えられたperson_t型のリストのうち血液型がAの人の数を返す *)

type person_t = {
  name : string;
  bloodtype : string;
}

let lst01 = []
let lst02 = [
  { name = "hoge"; bloodtype = "A" }
]
let lst02 = [
  { name = "hoge"; bloodtype = "A" };
  { name = "fuga"; bloodtype = "B" }
]
let lst03 = [
  { name = "hoge"; bloodtype = "O" };
  { name = "fuga"; bloodtype = "B" }
]
let lst04 = [
  { name = "hoge"; bloodtype = "A" };
  { name = "fuga"; bloodtype = "B" };
  { name = "bar"; bloodtype = "A" }
]

let rec count_ketsueki_A lst = match lst with
    [] -> 0
  | { bloodtype = b } :: rest -> if b = "A" then 1 + count_ketsueki_A rest
                                            else count_ketsueki_A rest

let test01 = count_ketsueki_A lst01 = 0
let test02 = count_ketsueki_A lst02 = 1
let test03 = count_ketsueki_A lst03 = 0
let test04 = count_ketsueki_A lst04 = 2
