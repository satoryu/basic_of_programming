(* 問題 22.1 *)

let count = ref (-1)

let gensym str = (
  count := !count + 1;
  str ^ (string_of_int !count);
)

let test1 = gensym "a" = "a0"
let test1 = gensym "x" = "x1"