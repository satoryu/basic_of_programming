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

(* 問題20.3 *)
(* 目的: 受け取ったrb_tree型 の赤黒木に、受け取ったキーと値を挿入する。既に同じキーが存在する場合、与えられた値で上書きする。*)
(* insert: rb_tree * 'a * 'b -> rb_tree *)

let check_root rb_tree = match rb_tree with
  Empty -> Empty
  | Node ( Node (tr_a, k_x, v_x, Red, tr_b), k_y, v_y, Red, tr_right) -> Node ( Node (tr_a, k_x, v_x, Red, tr_b), k_y, v_y, Black, tr_right)
  | Node ( tr_left, k_y, v_y, Red, Node (tr_c, k_z, v_z, Red, tr_d)) -> Node ( tr_left, k_y, v_y, Black, Node (tr_c, k_z, v_z, Red, tr_d))
  | Node (tr_a, k, v, c, tr_b) -> rb_tree

let insert rb_tree k v =
  let rec insert_rec tree _k _v = match tree with
    Empty -> Node (Empty, _k, _v, Red, Empty)
    | Node (tr_left, key, value, color, tr_right) ->
      if _k = key then Node (tr_left, key, _v, color, tr_right)
      else if _k < key then
          balance (Node (insert_rec tr_left _k _v, key, value, color, tr_right))
        else
          balance (Node (tr_left, key, value, color, insert_rec tr_right _k _v))
  in ( check_root (insert_rec rb_tree k v))

let test1 = (insert Empty 1 'v') = Node (Empty, 1, 'v', Red, Empty)
let test2 = (insert (Node (Empty, 2, 'v', Black, Empty)) 1 'w') = Node ( Node (Empty, 1, 'w', Red, Empty), 2, 'v', Black, Empty)
let test2 = (insert (Node (Empty, 2, 'v', Black, Empty)) 1 'w') = Node ( Node (Empty, 1, 'w', Red, Empty), 2, 'v', Black, Empty)
let test3 = (insert (Node (Empty, 1, 'v', Red, Empty)) 2 'w') = Node (Empty, 1, 'v', Black, Node (Empty, 2, 'w', Red, Empty))
let test4 = (insert (Node (Node (Empty, 2, 'y', Red, Empty), 3, 'z', Black, Empty)) 1 'x') = Node (Node (Empty, 1, 'x', Black, Empty), 2, 'y', Red, Node (Empty, 3, 'z', Black, Empty))


(* 問題 20.4 *)
(* 目的: rb_tree_t型として与えられた赤黒木の中に、与えられたキーを探し、存在すればそのキーに対応する値を返し、無ければ例外Not_foundを返す *)
(* search: rb_tree_t * 'a -> 'b *)

let search rb_tree key =
  let rec search_rec tree k = match tree with
    Empty -> None
    | Node (tree_left, ky, value, color, tree_right) ->
      if ky = k then Some (value)
      else if k < ky then search_rec tree_left k else search_rec tree_right k
    in (
      match search_rec rb_tree key with
      None -> raise Not_found
      | Some (v) -> v
    )


let test5 = search (Node (Empty, 1, 'v', Black, Empty)) 1 = 'v'
let test6 = search (Node (Node (Empty, 1,'w', Red, Empty), 2, 'v', Black, Empty)) 1 = 'w'
(* let test7 = search (Node (Node (Empty, 1,'w', Red, Empty), 2, 'v', Black, Empty)) 3 = 'w' *)
let test8 = search Empty 3 = 'w'
