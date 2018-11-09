
open Tracing_types

(*
a x^2 + b x + c = 0  (a > 0)
x^2 + b/a x + c/a = 0
(x + b/2a)^2 + c/a - b^2/4a^2 = 0
b^2/4a^2 - c/a >= 0
Delta = (b^2 - 4ac) >= 0
x = - b/2a + sqrt{(b^2 - 4ac)}/2a
x = (sqrt{Delta} - b)/2a
*)

val q_zeros: float -> float -> float -> float list

(*
  Intersection with ball:
  ray is: ro + t*rd  (t >= 0)
  ball is: (v - bo)^2 = br^2
  (ro + t*rd - bo)^2 = br^2
  ((ro - bo) + t*rd)^2 = br^2
  |ro - bo|^2 + 2 t (ro - bo, rd) + t^2 |rd|^2 = br^2
  |rd|^2 t2 + 2 (ro - bo, rd) t + |ro - bo|^2 - br^2 = 0
  Delta = 4 (ro - bo, rd)^2 - 4 |rd|^2 (|ro - bo|^2 - br^2) >= 0
  t = (sqrt{Delta} - 2 (ro - bo, rd))/2|rd|^2
*)
val intersect: ray -> w_object -> intersection list

val colorize_point: world -> ray -> color_point

val colorize: image_plane -> world -> eye -> color_point Seq.t

val run: unit -> unit
