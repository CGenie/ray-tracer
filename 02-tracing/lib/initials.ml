
open Gg
(* open Gg.V3 *)

open Tracing_types


let default_phong: phong_t = {
  ambient=0.1;
  diffuse=0.9;
  specular=0.9;
  shininess=200.0
}
let initial_eye: eye = {
  origin=P3.v 0.0 0.0 (-2.0)
}
let initial_image_plane: image_plane = {
  ll=P3.v (-5.) (-5.) (-1.);
  ur=P3.v 5. 5. (-1.)
}
let initial_ball: shape = Ball {
  origin=P3.v 0.0 0.0 1.0;
  radius=1.0
}
let initial_w_ball: w_object = {
  shape=initial_ball;
  phong=default_phong;
  color=Color.red
}
let initial_world: world = {
  objects=[initial_w_ball];
  light=P3.v 1.0 1.0 0.0;
  background_color=Color.black
}
let x_initial_rays: int = 4
let y_initial_rays: int = 4

(** [get_rays] returns rays emanating from [o] and intersecting the [ip:image_plane].
    The origin of each ray is the point where it intersects the plane. **)
let get_rays (o:p3) (ip:image_plane) (nx:int) (ny:int) =
  (* TODO: handle tilted plane, i.e. one that doesn't have z = const *)
  let nxp = nx + 1
  and nyp = ny + 1 in
  let n: int = nxp * nyp
  and x_min = V3.x ip.ll
  and y_min = V3.y ip.ll
  and ip_z = P3.z ip.ll in  (* TODO: ip.ll.z = ip.ur.z assumption *)
  let x_size = (V3.x ip.ur) -. x_min
  and y_size = (V3.y ip.ur) -. y_min in
  let rays: ray list = List.init n (fun idx ->
    let x_f = (float_of_int @@ idx mod nxp) /. (float_of_int nxp)
    and y_f = (float_of_int @@ idx / nxp) /. (float_of_int nyp) in
    let dd = P3.v (x_min +. x_f*.x_size) (y_min +. y_f*.y_size) ip_z in
    {origin=dd; direction=V3.unit (V3.sub dd o)}
  ) in
  rays