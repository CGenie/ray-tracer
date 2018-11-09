
(* open Gg *)
open Gg.V3

open Formatters
open Algorithms
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

let rec intersect ({origin=ro; direction=rd} as r:ray) wo =
  (* TODO: ray direction should be normalized, add an assertion *)
  let rd2 = norm2 rd in

  match wo.shape with
  | Ball{origin=bo; radius=br} ->
    let ro_bo = ro - bo in
    let a = rd2
    and b = 2.*.(dot ro_bo rd)
    and c = (norm2 ro_bo) -. br*.br in
    let zeros = q_zeros a b c in (
    match zeros with
      [] -> []
    | z::_ -> [{normal=unit (ro_bo); point=(ro + z*rd); ray=r; w_object=wo}]
    )
  | Affine{m=_; inv_m=inv_m; shape=s} ->
    let inv_ray = {origin=Gg.P3.tr inv_m ro; direction=Gg.V3.tr inv_m rd} in
    let intersections = intersect inv_ray {wo with shape=s} in
    List.map (fun i -> {i with w_object=wo}) intersections

(** Phong model coloring function **)
let phong_color (material_color: Gg.color) (material_phong: phong_t) (light_: lighting) (point: Gg.p3) (eyev: Gg.v3) (normalv: Gg.v3) =
  match light_ with
    PointLight light -> Gg.Color.with_a (
      let effective_color = Gg.V4.mul light.intensity material_color in
      let ambient =  Gg.V4.smul material_phong.ambient effective_color in
      let lightv = unit @@ light.position - point in
      let light_dot_normal = dot lightv normalv in
      if light_dot_normal < 0.0 then
        ambient  (* Not illuminated -- light behind the surface -- diffuse = specular = Color.black *)
      else (
        let diffuse = Gg.V4.smul (material_phong.diffuse *. light_dot_normal) effective_color in
        let reflectv = reflect (neg lightv) normalv in
        let reflect_dot_eye = (dot reflectv eyev) ** material_phong.shininess in
        let specular = if reflect_dot_eye <= 0.0 then
          Gg.Color.black
        else
          Gg.V4.smul (material_phong.specular *. reflect_dot_eye) light.intensity
        in
        Gg.V4.add ambient @@ Gg.V4.add diffuse specular
      )
    ) 1.0

(* phong_color tests *)
let%test_module _ = (module struct
  let m_phong: phong_t = {
    ambient=0.1;
    diffuse=0.9;
    specular=0.9;
    shininess=200.0
  }
  let m_color = Gg.Color.v 1.0 1.0 1.0 1.0
  let position = Gg.P3.v 0.0 0.0 0.0

  (* light behind the eye *)
  let%test _ = let eyev = v 0.0 0.0 (-1.0) in
    let normalv = v 0.0 0.0 (-1.0) in
    let light = PointLight {position=Gg.P3.v 0.0 0.0 (-10.0); intensity=Gg.Color.v 1.0 1.0 1.0 1.0} in
    let ph_light = phong_color m_color m_phong light position eyev normalv in
    let expected = Gg.Color.v 1.9 1.9 1.9 1.0 in
    let err = Gg.V4.norm @@ Gg.V4.sub ph_light expected in
    err < 0.00001

  (* 45 deg angle *)
  let%test _ = let eyev = v 0.0 (0.5 *. (sqrt 2.0)) (-0.5 *. (sqrt 2.0)) in
    let normalv = v 0.0 0.0 (-1.0) in
    let light = PointLight {position=Gg.P3.v 0.0 0.0 (-10.0); intensity=Gg.Color.v 1.0 1.0 1.0 1.0} in
    let ph_light = phong_color m_color m_phong light position eyev normalv in
    let expected = Gg.Color.v 1.0 1.0 1.0 1.0 in
    let err = Gg.V4.norm @@ Gg.V4.sub ph_light expected in
    err < 0.00001

  (* 45 deg angle again *)
  let%test _ = let eyev = v 0.0 0.0 (-1.0) in
    let normalv = v 0.0 0.0 (-1.0) in
    let light = PointLight {position=Gg.P3.v 0.0 10.0 (-10.0); intensity=Gg.Color.v 1.0 1.0 1.0 1.0} in
    let ph_light = phong_color m_color m_phong light position eyev normalv in
    let expected = Gg.Color.v 0.7364 0.7364 0.7364 1.0 in
    let err = Gg.V4.norm @@ Gg.V4.sub ph_light expected in
    err < 0.00001

  (* 45 deg angle again *)
  let%test _ = let eyev = v 0.0 (-0.5 *. (sqrt 2.0)) (-0.5 *. (sqrt 2.0)) in
    let normalv = v 0.0 0.0 (-1.0) in
    let light = PointLight {position=Gg.P3.v 0.0 10.0 (-10.0); intensity=Gg.Color.v 1.0 1.0 1.0 1.0} in
    let ph_light = phong_color m_color m_phong light position eyev normalv in
    let expected = Gg.Color.v 1.6364 1.6364 1.6364 1.0 in
    let err = Gg.V4.norm @@ Gg.V4.sub ph_light expected in
    err < 0.00001

  (* light behind the surface *)
  let%test _ = let eyev = v 0.0 0.0 (-1.0) in
    let normalv = v 0.0 0.0 (-1.0) in
    let light = PointLight {position=Gg.P3.v 0.0 0.0 10.0; intensity=Gg.Color.v 1.0 1.0 1.0 1.0} in
    let ph_light = phong_color m_color m_phong light position eyev normalv in
    let expected = Gg.Color.v 0.1 0.1 0.1 1.0 in
    let err = Gg.V4.norm @@ Gg.V4.sub ph_light expected in
    Printf.printf "ph_light: %s\n" (format_v4 ph_light);
    Printf.printf "expected: %s\n" (format_v4 expected);
    err < 0.00001
end)

(** "Shoot" a ray into the world and fetch its colour.
    We need to take the "first" intersection, i.e. the one closest to the ray origin. **)
let colorize_point (w: world) (r: ray) =
  let intersections_mapped = List.rev_map (intersect r) w.objects
  and comparator (a: intersection) (b: intersection) =
    let d = (norm2 @@ r.origin - a.point) -. (norm2 @@ r.origin - b.point) in
    if d < 0. then (-1)
    else if d > 0. then 1
    else 0 in
  let intersections =  List.fold_left List.append [] intersections_mapped in
  let intersections_sorted = List.sort comparator intersections in
  let c =
    match intersections_sorted with
      [] -> w.background_color
    | (i::_) -> phong_color i.w_object.color i.w_object.phong w.light i.point (unit @@ neg i.ray.direction) i.normal in (* i.w_object.color -- for chapter 5 solution with only a red ball, no Phong *)
  {point=r.origin; color=c}

let colorize (ip: image_plane) (w: world) (e: eye) =
  let rays = get_rays e.origin ip x_initial_rays y_initial_rays in
  (* Printf.printf "num rays: %d\n" (List.length rays); *)
  (* List.iter (fun r -> Printf.printf "%s\n" (format_ray r)) rays; *)
  (* List.rev_map (colorize_point w) rays *)
  Seq.map (colorize_point w) rays


(* Draw sphere, paint it in a simple way *)
(* This is "Putting it together" task at end of Chapter 5 *)
let simple_scene () =
  let w = initial_world
  and ip = initial_image_plane
  and e = initial_eye in
  let pts = colorize ip w e in
  Printf.printf "Colorize done\n";
  Drawing.render_points pts ~output:"./output/simple-scene.png"

(* external float_compare_noalloc : float -> float -> int = *)
(*   "float_compare_noalloc_stub" "mystubs" *)

let run () =
  simple_scene ();
  Dumb.scene ()
  (* Printf.printf "Float compare: %d\n" (float_compare_noalloc 1.0 2.0) *)
