(* tracing.ml *)

open Gg.V3

open Formatters
open Algorithms
open Tracing_types
open Initials

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

let image_plane_of_camera c =
  let mid_p = c.eye + (smul c.distance c.direction) in  (* middle point of image plane *)
  (* find 2 vetors perpendicular to c.direction: one going "right", one going "up" *)
  let x, y, z = to_tuple c.direction in
  let vec_r = unit @@ if z < 0.0 then
    Gg.V3.v (-.z) 0.0 x
  else
    Gg.V3.v z 0.0 (-.x)
  in
  let vec_u = unit @@ if z < 0.0 then
    Gg.V3.v 0.0 (-.z) y
  else
    Gg.V3.v 0.0 z (-.y)
  in
  (* lower-left point of image plane *)
  let ll = mid_p - (smul (0.5 *. c.width) vec_r) - (smul (0.5 *. c.height) vec_u) in
  (* upper-right point of image plane *)
  let ur = mid_p + (smul (0.5 *. c.width) vec_r) + (smul (0.5 *. c.height) vec_u) in
  {ll=ll; ur=ur}

(* [image_plane_of_camera] tests *)
let%test_module _ = (module struct
  let%test _ =
    let c: camera = {
      eye=Gg.P3.v 0.0 0.0 (-2.0);
      direction=Gg.V3.unit @@ Gg.V3.v 0.0 0.0 1.0;
      distance=1.0;
      width=4.0;
      height=4.0;
    } in
    let ip = image_plane_of_camera c in
    let expected_ll = Gg.P3.v (-2.) (-2.) (-1.) in
    let expected_ur = Gg.P3.v 2. 2. (-1.) in
    let err = (Gg.V3.norm (ip.ll - expected_ll)) +. (Gg.V3.norm (ip.ur - expected_ur)) in
    Printf.printf "ll: %s; expected_ll: %s\n" (format_p3 ip.ll) (format_p3 expected_ll);
    Printf.printf "ur: %s; expected_ur: %s\n" (format_p3 ip.ur) (format_p3 expected_ur);
    err < 0.001
end)

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

(* [phong_color] tests *)
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
    err < 0.00001
end)

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
    | (i::_) ->
      let vv = (unit @@ neg i.ray.direction) in
      (* i.w_object.color -- for chapter 5 solution with only a red ball, no Phong *)
      List.fold_left (fun acc l -> Gg.V4.add acc (phong_color i.w_object.color i.w_object.phong l i.point vv i.normal))
                     Gg.Color.black
                     w.lights
  in
  {point=r.origin; color=Gg.Color.with_a c 1.0}

let colorize (ip: image_plane) (w: world) (e: eye) =
  let rays = get_rays e ip x_initial_rays y_initial_rays in
  (* Printf.printf "num rays: %d\n" (List.length rays); *)
  (* List.iter (fun r -> Printf.printf "%s\n" (format_ray r)) rays; *)
  (* List.rev_map (colorize_point w) rays *)
  Seq.map (colorize_point w) rays


let simple_scene () =
  let w = initial_world in
  let c = initial_camera in
  let ip = image_plane_of_camera c in
  Printf.printf "Colorizing...\n";
  (* let pts = colorize_parallel ip w e in *)
  let pts = colorize ip w c.eye in
  Printf.printf "Colorize done\n";
  Drawing.render_points pts ~output:"./output/simple-scene.png"

(* external float_compare_noalloc : float -> float -> int = *)
(*   "float_compare_noalloc_stub" "mystubs" *)

let run () =
  simple_scene ();
  Dumb.scene ()
  (* Printf.printf "Float compare: %d\n" (float_compare_noalloc 1.0 2.0) *)
