(** Tracing a simple 3D scene.
    *)

open Tracing_types

(** {2:tracing-types Tracing types}

{3:tracing-types-description Basic types for the ray tracer}
{{!Tracing__.Tracing_types}Tracing_types}
*)

(** {2:algorithms Algorithms}

{3:algorithms-description Various mathematical algorithms.}
{{!Tracing__.Algorithms}Algorithms}
*)

(** {2:drawing Drawing}

{3:drawing-description Drawing functions}
{{!Tracing__.Drawing}Drawing}
*)

(** {2:formatters Formatters}

{3:formatters-description Formatting functions for [gg]/[vg]}
{{!Tracing__.Formatters}Formatters}
*)

(** {2:initials Initials}

{3:initials-description Initial values for [simple_scene] ray tracing}
{{!Tracing__.Initials}Initials}
*)


val intersect: ray -> w_object -> intersection list
(** [intersect r w_obj]
  Intersection with ball:
  ray is: [ro + t*rd  (t >= 0)]
  ball is: [(v - bo)^2 = br^2]
  {[
  (ro + t*rd - bo)^2 = br^2
  ((ro - bo) + t*rd)^2 = br^2
  |ro - bo|^2 + 2 t (ro - bo, rd) + t^2 |rd|^2 = br^2
  |rd|^2 t2 + 2 (ro - bo, rd) t + |ro - bo|^2 - br^2 = 0
  Delta = 4 (ro - bo, rd)^2 - 4 |rd|^2 (|ro - bo|^2 - br^2) >= 0
  t = (sqrt{Delta} - 2 (ro - bo, rd))/2|rd|^2
  ]}
*)

val image_plane_of_camera: camera -> image_plane

val phong_color: Gg.color -> phong_t -> lighting -> Gg.p3 -> Gg.v3 -> Gg.v3 -> Gg.color
(** [phong_color material_color material_phong light point eyev normalv]
    Phong model coloring function *)

val colorize_point: world -> ray -> color_point
(** [colorize_point w r]
    "Shoot" a ray into the world and fetch its colour.
    We need to take the "first" intersection, i.e. the one closest to the ray origin. *)

val colorize: image_plane -> world -> eye -> color_point Seq.t

val simple_scene: unit -> unit
(** [simple_scene ()]
    Draw sphere, paint it in a simple way
    This is "Putting it together" task at end of Chapter 5 *)

val run: unit -> unit
