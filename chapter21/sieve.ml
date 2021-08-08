(* 問題 21.2 *)

let rec filter p lst = match lst with
  [] -> []
  | first :: rest -> if p first then first :: (filter p rest) else filter p rest

let rec length lst = match lst with
  [] -> 0
  | first :: rest -> 1 + length rest

let rec sieve lst = (
  print_string "Length of Args: ";
  print_int (length lst);
  print_newline ();
  match lst with
  [] -> []
  | first :: rest -> first :: (sieve (filter (fun item -> item mod first != 0) rest)))


let a = sieve [2; 3; 4; 5; 6; 7; 8; 9; 10]