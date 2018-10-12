
(* open Gg *)
open Gg.V3

open Formatters
open Tracing_types
open Initials

(*
a x^2 + b x + c = 0  (a > 0)
x^2 + b/a x + c/a = 0
(x + b/2a)^2 + c/a - b^2/4a^2 = 0
b^2/4a^2 - c/a >= 0
Delta = (b^2 - 4ac) >= 0
x = - b/2a + sqrt{(b^2 - 4ac)}/2a
x = (+/- sqrt{Delta} - b)/2a
*)

(** Zeros of a quadratic function a x^2 + b x + c. **)
let q_zeros a b c =
  match a with
    0.0  -> [(-1.)*.c/.b]
  | _    ->
    let delta = b*.b -. 4.*.a*.c in
    if delta < 0. then []
    else
      let sdelta = 0.5*.(sqrt delta)/.a
      and b2a = (-0.5)*.b/.a in
      [b2a -. sdelta; b2a +. sdelta]

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

let intersect ({origin=ro; direction=rd}:ray) wo =
  (* TODO: ray direction should be normalized, add an assertion *)
  let rd2 = norm2 rd in

  match wo.shape with
  | Ball{origin=bo; radius=br} ->
    let ro_bo = sub ro bo in
    let a = rd2
    and b = 2.*.(dot ro_bo rd)
    and c = (norm2 ro_bo) -. br*.br in
    let zeros = q_zeros a b c in (
    match zeros with
      [] -> []
    | z::_ -> [{point=(ro + z*rd); w_object=wo}]
    )

(** "Shoot" a ray into the world and fetch its colour.
    We need to take the "first" intersection, i.e. the one closest to the ray origin. **)
let colorize_point (w: world) (r: ray) =
  let intersections = List.concat @@ List.map (intersect r) w.objects
  and comparator (a: intersection) (b: intersection) =
    let d = (norm2 @@ sub r.origin a.point) -. (norm2 @@ sub r.origin b.point) in
    if d < 0. then (-1)
    else if d > 0. then 1
    else 0 in
  let intersections_sorted = List.sort comparator intersections in
  let c =
    match intersections_sorted with
      [] -> w.background_color
    | (i::_) -> i.w_object.color in  (* TODO: actual ray-tracing coloring (Phong or other) *)
  {point=r.origin; color=c}

let colorize (ip: image_plane) (w: world) (e: eye) =
  let rays = get_rays e.origin ip x_initial_rays y_initial_rays in
  Printf.printf "num rays: %d\n" (List.length rays);
  List.iter (fun r -> Printf.printf "%s\n" (format_ray r)) rays;
  List.map (colorize_point w) rays

(* Draw sphere, paint it in a simple way *)
(* This is "Putting it together" task at end of Chapter 5 *)
let simple_scene () =
  let w = initial_world
  and ip = initial_image_plane
  and e = initial_eye in
  let pts = colorize ip w e in
  Drawing.render_points pts ~output:"./output/simple-scene.png";
  Printf.printf "num pts: %d\n" (List.length pts)

let run () =
  simple_scene ();
  Dumb.scene ()
