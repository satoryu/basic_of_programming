(* 問題 15.3 *)
(* 目的: 自然数 n 以下の素数全てを求める *)
(* sieve: int -> int lst *)

let rec sequence n = if n = 2 then [2] else n :: (sequence (n - 1))
let rec filter p lst = match lst with
  [] -> []
  | first :: rest -> if p first then first :: (filter p rest) else filter p rest
let rec reverse lst = match lst with
  [] -> []
  | first :: rest -> (reverse rest) @ [first]

let rec sieve lst = match lst with
  [] -> []
  | first :: rest -> first :: (sieve (filter (fun item -> item mod first != 0) rest))

let seq = reverse (sequence 10)
let test0 = sieve seq = [2; 3; 5; 7]


let prime n =
  let seq = reverse (sequence n)
  in sieve seq

let test1 = prime 10 = [2; 3; 5; 7]
