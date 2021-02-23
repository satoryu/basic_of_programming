(* 問題8.1 *)

type book_t = {
  title : string;
  author : string;
  publisher : string;
  price : int;
  isbn : string;
}

(* 問題8.2 *)

type date_t = {
  year : int;
  month : int;
  day : int;
}

type okozukai_t = {
  item : string;
  price : int;
  shop : string;
  date : date_t;
}

(* 問題8.3 *)
type person_t = {
  height : float;
  weight : float;
  bloodtype : string;
  birthday : date_t;
}

let me = { height = 167.8; weight = 61.2; bloodtype = "AB"; birthday = { year = 1981; month = 7; day = 29} }
let mao = { height = 141.; weight = 30.2; bloodtype = "A"; birthday = { year = 2009; month = 4; day = 29} }
let kei = { height = 130.; weight = 40.2; bloodtype = "A"; birthday = { year = 2011; month = 10; day = 29} }
