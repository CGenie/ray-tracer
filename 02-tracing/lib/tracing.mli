
open Tracing_types

(**
  Intersection with ball:
  ray is: ro + t*rd  (t >= 0)
  ball is: (v - bo)^2 = br^2
  (ro + t*rd - bo)^2 = br^2
  ((ro - bo) + t*rd)^2 = br^2
  |ro - bo|^2 + 2 t (ro - bo, rd) + t^2 |rd|^2 = br^2
  |rd|^2 t2 + 2 (ro - bo, rd) t + |ro - bo|^2 - br^2 = 0
  Delta = 4 (ro - bo, rd)^2 - 4 |rd|^2 (|ro - bo|^2 - br^2) >= 0
  t = (sqrt{Delta} - 2 (ro - bo, rd))/2|rd|^2
**)
val intersect: ray -> w_object -> intersection list

val image_plane_of_camera: camera -> image_plane

val colorize_point: world -> ray -> color_point

val colorize: image_plane -> world -> eye -> color_point Seq.t

val run: unit -> unit
