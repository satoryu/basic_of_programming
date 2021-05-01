type tree_t = Empty
  | Leaf of int
  | Node of tree_t * int * tree_t

(* 問題 17.5 *)
(* 目的: 受け取ったtree_t型の木のそれぞれの節や葉を2倍にした値の木を返す *)
(* tree_double: tree_t -> tree_t *)

let rec tree_double tree = match tree with
  Empty -> Empty
  | Leaf (n) -> Leaf (2 * n)
  | Node (t1, n, t2) -> Node (tree_double t1, 2 * n, tree_double t2)

let test0 = tree_double Empty = Empty
let test1 = tree_double (Leaf (2)) = Leaf (4)
let test2 = tree_double (Node (Leaf (2), 1, Empty)) = (Node (Leaf(4), 2, Empty))

(* 問題 17.6 *)
(* 目的: 受け取ったtree_t型の木のそれぞれの節や葉の値に、与えられた関数fを適用した木を返す *)
(* tree_map: (int -> a') -> tree_t -> tree_t *)
let rec tree_map f tree = match tree with
  Empty -> Empty
  | Leaf (n) -> Leaf (f n)
  | Node (t1, n, t2) -> Node (tree_map f t1, (f n), tree_map f t2)

let test3 = tree_map (fun a -> a) Empty = Empty
let test4 = tree_map (fun a -> a + 1) (Leaf (2)) = Leaf (3)
let test5 = tree_map (fun a -> 2 * a) (Node (Leaf (1), 2, Empty)) = Node (Leaf (2), 4, Empty)

(* 問題 17.7 *)
(* 目的: tee_t型で与えられた木の中にある葉と節の数を数える *)
(* tree_length: tree_t -> int *)

let rec tree_length tree = match tree with
  Empty -> 0
  | Leaf (n) -> 1
  | Node (t1, n, t2) -> (tree_length t1) + (tree_length t2) + 1

let test6 = tree_length Empty = 0
let test7 = tree_length (Leaf (0)) = 1
let test8 = tree_length (Node (Leaf (0), 0, Empty)) = 2

(* 問題 17.8 *)
(* 目的: tree_t型で与えられた木の深さを求める *)
(* tree_depth: tree_t -> int *)

let rec tree_depth tree = match tree with
  Empty -> 0
  | Leaf (n) -> 0
  | Node (t1, n, t2) -> (max (tree_depth t1) (tree_depth t2)) + 1

let test9 = tree_depth Empty = 0
let test10 = tree_depth (Leaf (0)) = 0
let test11 = tree_depth (Node (Leaf (0), 1, Empty)) = 1
let test12 = tree_depth (Node (Empty, 1, Empty)) = 1
let test13 = tree_depth (Node (Node (Leaf(1), 2, Leaf (3)), 1, Leaf (1))) = 2