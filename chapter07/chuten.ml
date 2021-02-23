(* 問題7.4 *)
(* 目的: 2つの平面座標点を表す組を受け取ったら、その中点の座標を返す *)
(* chuten : ((float * float) * (float * float)) -> (float * float) *)

let chuten ((x1, y1), (x2, y2)) = ((x1 +. x2 ) /. 2., (y1 +. y2) /. 2.)

let test1 = chuten((0., 0.), (0., 0.)) = (0., 0.)
let test2 = chuten((0., 0.), (1., 1.)) = (0.5, 0.5)
let test3 = chuten((1., 1.), (0., 0.)) = (0.5, 0.5)
let test4 = chuten((-1., 1.), (0., 0.)) = (-0.5, 0.5)
let test5 = chuten((1., -1.), (0., 0.)) = (0.5, -0.5)