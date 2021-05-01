(* 問題 17.9 *)

type 'a tree_t = Empty
  | List of 'a
  | Node of 'a tree_t * 'a * 'a tree_t

let rec sum_tree tree = match tree with
  Empty -> 0
  | List (n) -> n
  | Node (t1, n, t2) -> (sum_tree t1) + n + (sum_tree t2)

(* sum_tree の型は、 int tree_t -> int。再起で利用している + 演算子がintのためそれにより型が'aの型が確定してしまう。 *)