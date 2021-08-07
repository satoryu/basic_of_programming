(* 問題 20.1 *)
type color_t = Red | Black

type ('a, 'b) rb_tree_t = Empty
  | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

(* 問題 20.2 *)
let balance tree = match tree with
  Empty -> Empty
  | Node ((Node (Node (tr_a, k_x, v_x, Red, tr_b), k_y, v_y, Red, tr_c)), k_z, v_z, Black, tr_d)
    | Node ((Node (tr_a, k_x, v_x, Red, Node (tr_b, k_y, v_y, Red, tr_c))), k_z, v_z, Black, tr_d)
    | Node (tr_a, k_x, v_x, Black, (Node (Node (tr_b, k_y, v_y, Red, tr_c), k_z, v_z, Red, tr_d)))
    | Node (tr_a, k_x, v_x, Black, (Node (tr_b, k_y, v_y, Red, Node (tr_c, k_z, v_z, Red, tr_d))))
      -> Node (Node (tr_a, k_x, v_x, Black, tr_b), k_y, v_y, Red, Node (tr_c, k_z, v_z, Black, tr_d))
  | Node (tr_l, k, v, c, tr_r) -> Node (tr_l, k, v, c, tr_r)

let test1 = balance Empty = Empty
let test2 = (balance (Node ((Node (Node (Empty, "k_x", 1, Red, Empty), "k_y", 2, Red, Empty)), "k_z", 3, Black, Empty))) = Node (Node (Empty, "k_x", 1, Black, Empty), "k_y", 2, Red, Node (Empty, "k_z", 3, Black, Empty))
let test3 = (balance (Node ((Node (Empty, "k_x", 1, Red, Node (Empty, "k_y", 2, Red, Empty))), "k_z", 3, Black, Empty))) = Node (Node (Empty, "k_x", 1, Black, Empty), "k_y", 2, Red, Node (Empty, "k_z", 3, Black, Empty))
let test4 = (balance (Node (Empty, "k_x", 1, Black, (Node (Node (Empty, "k_y", 2, Red, Empty), "k_z", 3, Red, Empty))))) = Node (Node (Empty, "k_x", 1, Black, Empty), "k_y", 2, Red, Node (Empty, "k_z", 3, Black, Empty))
let test5 = (balance (Node (Empty, "k_x", 1, Black, (Node (Empty, "k_y", 2, Red, (Node (Empty, "k_z", 3, Red, Empty))))))) = Node (Node (Empty, "k_x", 1, Black, Empty), "k_y", 2, Red, Node (Empty, "k_z", 3, Black, Empty))
let test6 = (balance (Node (Empty, "k", 1, Red, Empty))) = Node (Empty, "k", 1, Red, Empty)
let test7 = (balance (Node (Empty, "k", 1, Black, Empty))) = Node (Empty, "k", 1, Black, Empty)
