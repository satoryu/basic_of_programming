(* 問題14.8 *)
(fun x -> x * x - 1) 5

(* 問題14.9 *)

type person_t = {
  name : string
};;

(fun n -> n.name) { name = "Suzuka Nakamoto" }

(* 問題 14.10 *)

let rec fold_right f lst init = match lst with
  [] -> init
  | first :: rest -> f first (fold_right f rest init)

let rec filter p lst = match lst with
  [] -> []
  | first :: rest -> if p first then first :: (filter p rest)
                                else filter p rest

let rec length lst = match lst with
  [] -> 0
  | first :: rest -> 1 + length rest

let rec map f lst = match lst with
  [] -> []
  | first :: rest -> f first :: (map f rest)

let even lst = filter (fun n -> (n mod 2) = 0) lst

let test0 = even [3; 4; 5; 2] = [4; 2]

type gakusei_t = { seiseki : string }
let count_A lst = length (filter (fun g -> g.seiseki = "A") lst)

let test1 = count_A [{seiseki = "B"}; {seiseki = "A"}; {seiseki = "A"}] = 2

let concat lst = fold_right (fun a b -> a ^ b) lst ""
let test2 = concat ["BABY"; "METAL"; "DEATH"] = "BABYMETALDEATH"

type gakusei_t = { score : int }
let gakusei_sum lst = fold_right (fun x y -> x + y) (map (fun g -> g.score) lst) 0

let test3 = gakusei_sum [{score = 10}; {score = 100}] = 110