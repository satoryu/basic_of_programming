(* 問題 9.1 *)

let seasons = "春" :: "夏" :: "秋" :: "冬" :: [];;

(* 問題 9.2 *)
type date_t = {
  year : int;
  month : int;
  day : int;
}

type person_t = {
  height : float;
  weight : float;
  bloodtype : string;
  birthday : date_t;
}

let members = { height = 130.0; weight = 40.5; bloodtype = "B"; birthday = { year = 2011; month = 10; day = 29 }} :: { height = 140.0; weight = 30.5; bloodtype = "A"; birthday = { year = 2009; month = 4; day = 29 }} :: { height = 168.0; weight = 60.5; bloodtype = "AB"; birthday = { year = 1981; month = 7; day = 29 }} :: [];;
