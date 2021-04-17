(* 問題14.5 *)

let rec filter p lst = match lst with
  [] -> []
  | first :: rest -> if p first then first :: (filter p rest)
                                else filter p rest

let even lst =
  let is_even n = (n mod 2) = 0 in filter is_even lst

let test0 = even [3; 4; 5; 2] = [4; 2]

type gakusei_t = {
  seiseki : string
}

let count_A lst =
  let rec length lstl = match lstl with
    [] -> 0
    | first :: rest -> 1 + length rest
    in (
      let is_A g = match g with
        { seiseki = s } -> s = "A"
      in length (filter is_A lst)
    )

let test1 = count_A [{seiseki = "B"}; {seiseki = "A"}; {seiseki = "A"}] = 2

let rec fold_right f lst init = match lst with
  [] -> init
  | first :: rest -> f first (fold_right f rest init)

let concat lst =
  let concat_two_string a b = a ^ b in fold_right concat_two_string lst ""

let test2 = concat ["BABY"; "METAL"; "DEATH"] = "BABYMETALDEATH"

type gakusei_t = {
  score : int
}

let rec map f lst = match lst with
  [] -> []
  | first :: rest -> f first :: (map f rest)

let gakusei_sum lst =
  let add x y = x + y in (
    let gakusei_score g = match g with
      { score = s } -> s
    in fold_right add (map gakusei_score lst) 0
  )

let test3 = gakusei_sum [{score = 10}; {score = 100}] = 110

(* 問題14.6 *)

let rec length lst = match lst with
    [] -> 0
    | first :: rest -> 1 + length rest

let count lst seiseki0 =
  let is_seiseki0 g = match g with
    { score = s } -> s = seiseki0
  in length (filter is_seiseki0 lst)

let test4 = count [] 100 = 0
let test5 = count [{score = 10}] 100 = 0
let test6 = count [{score = 100}] 100 = 1
let test7 = count [{score = 100}; {score = 10}] 100 = 1
let test8 = count [{score = 100}; {score = 10}; {score = 100}] 100 = 2