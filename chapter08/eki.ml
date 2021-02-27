(* 問題 8.5 *)

type ekimei_t = {
  kanji : string; (* 漢字表記 *)
  kana : string;  (* かな表記 *)
  romaji : string; (* ローマ字表記 *)
  shozoku : string; (* 所属する路線名 *)
}

(* 問題 8.6 *)
(* 目的: 駅名レコードekimei_tを与えられると、所属路線名、駅名とかな表記を表示する *)
(* hyoji: ekimei_t -> string *)
let hyoji ekimei = match ekimei with
  { kanji = k; kana = kn; shozoku = s} -> s ^ ", " ^ k ^ "（" ^ kn ^ "）"

let test1 = hyoji { kanji = "梅屋敷"; kana = "うめやしき"; romaji = "Umeyashiki"; shozoku = "京急本線" } = "京急本線, 梅屋敷（うめやしき）"

(* 問題 8.7 *)

type ekikan_t = {
  kiten : ekimei_t; (* 起点 *)
  shuten : ekimei_t; (* 終点 *)
  kyori : float; (* 距離 *)
  jikan : int; (* 所要時間 *)
}

let hyoji_ekikan ekikan = match ekikan with
  { kiten = k; shuten = s; kyori = ky } -> "（起点: " ^ hyoji k ^ "） - " ^ string_of_float ky ^ "km - " ^ "（終点: " ^ hyoji s ^ "）"

let ekikan = {
  kiten = { kanji = "梅屋敷"; kana = "うめやしき"; romaji = "Umeyashiki"; shozoku = "京急本線" };
  shuten = { kanji = "大森町"; kana = "おおもりまち"; romaji = "Omorimachi"; shozoku = "京急本線" };
  kyori = 0.8;
  jikan = 3;
};;

hyoji_ekikan ekikan;;

let test2 = hyoji_ekikan ekikan = "（起点: 京急本線, 梅屋敷（うめやしき）） - 0.8km - （終点: 京急本線, 大森町（おおもりまち））"
