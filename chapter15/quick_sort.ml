(* 問題 15.1 *)

let rec filter p lst = match lst with
  [] -> []
  | first :: rest -> if p first then first :: (filter p rest)
  else filter p rest

let test0 = filter (fun n -> n > 0) [0; 1; -1; 2] = [1; 2]


let rec quick_sort lst =
  let take n lst p = filter (fun item -> p item n) lst
  in let take_less_equal n lst = take n lst (<=)
  in let take_greater n lst = take n lst (>)
  in match lst with
    [] -> []
    | first :: rest -> quick_sort (take_less_equal first rest)
                       @ [first]
                       @ quick_sort (take_greater first rest)

let test1 = quick_sort [1; 2] = [1; 2]
let test2 = quick_sort [2; 1] = [1; 2]
let test3 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9]
let test4 = quick_sort [5; 5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 5; 8; 9]
