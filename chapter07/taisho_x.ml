(* 問題 7.3 *)
(* 目的: 平面座標のx座標、y座標を表す組を受け取り、x軸について対象な点を返す *)
(* taisho_x : (int * int) -> (int * int) *)

let taisho_x (x, y) = (x, -y)

let test1 = taisho_x (0, 0) = (0, 0)
let test2 = taisho_x (0, 1) = (0, -1)
let test3 = taisho_x (1, 1) = (1, -1)
let test4 = taisho_x (1, -1) = (1, 1)