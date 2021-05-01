(* 問題 17.1 *)

type nengou_t = Meiji of int (* 明治 *)
  | Taisho of int (* 大正 *)
  | Showa of int (* 昭和 *)
  | Heisei of int (* 平成 *)
  | Reiwa of int (* 令和 *)

let to_seireki nengou = match nengou with
  Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1988
  | Reiwa (n) -> n + 2018

(* 目的: nengou_t型で受け取った誕生年と現在の年から年齢を求める *)
let nenrei birth current = (-) (to_seireki current) (to_seireki birth)

let test0 = nenrei (Showa (56)) (Reiwa (3)) = 40

(* 問題 17.2 *)

type year_t = January
  | Feburuary
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

(* 問題 17.3 *)
type seiza_t = Aries
  | Taurus
  | Gemini
  | Cancer
  | Leo
  | Virgo
  | Libra
  | Scorpio
  | Sagittarius
  | Capricorn
  | Aquarius
  | Pisces

(* 問題 17.4 *)
let seiza year = match year with
  January -> Aquarius
  | Feburuary -> Pisces
  | March -> Aries
  | April -> Taurus
  | May -> Gemini
  | June -> Cancer
  | July -> Leo
  | August -> Virgo
  | September -> Libra
  | October -> Scorpio
  | November -> Sagittarius
  | December -> Capricorn

let test1 = seiza July = Leo
