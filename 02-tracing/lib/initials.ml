(* initials.ml *)

open Gg

open Tracing_types


let default_phong: phong_t = {
  ambient=0.1;
  diffuse=0.9;
  specular=0.9;
  shininess=200.0
}
let initial_camera: camera = {
  eye=P3.v 0.0 0.0 (-3.0);
  direction=Gg.V3.unit @@ V3.v 0.0 0.0 1.0;
  distance=1.0;
  width=4.0;
  height=4.0
}
let initial_ball: shape = Ball {
  origin=P3.v 0.0 0.0 1.0;
  radius=1.8
}
let ball_tr = Gg.M4.scale3 @@ V3.v 2.0 1.0 1.0
let initial_w_affine_ball: w_object = {
  shape=Affine{m=ball_tr; inv_m=Gg.M4.inv ball_tr; shape=initial_ball};
  phong=default_phong;
  color=Color.v 1.0 0.2 1.0 1.0
}
let ball_tr2 = Gg.M4.move3 @@ V3.v 0.0 1.5 0.0
let initial_w_ball: w_object = {
  shape=Affine{m=ball_tr2; inv_m=Gg.M4.inv ball_tr2; shape=initial_ball};
  phong=default_phong;
  color=Color.v 1.0 0.2 0.5 1.0
}
let first_light: lighting = PointLight {position=P3.v (-10.0) 10.0 (-10.0); intensity=Color.white}
let second_light: lighting = PointLight {position=P3.v (-10.0) 0.0 (-10.0); intensity=Color.white}
let initial_world: world = {
  objects=[initial_w_affine_ball; initial_w_ball];
  lights=[first_light; second_light];
  background_color=Color.black
}
let x_initial_rays: int = 300
let y_initial_rays: int = 300

let get_rays (o:p3) (ip:image_plane) (nx:int) (ny:int) =
  let nxp = nx + 1
  and nyp = ny + 1 in
  let dx = 1.0 /. (float_of_int nxp)
  and dy = 1.0 /. (float_of_int nyp) in
  (* assumption is that the image_plane is "straight", i.e. not tilted around the z-axis *)
  let ul = P3.v (V3.x ip.ll) (V3.y ip.ur) (V3.z ip.ur) in
  let vr = V3.sub ip.ur ul     (* "right" vector *)
  and vu = V3.sub ul ip.ll in  (* "upper" vector *)
  let vrs = V3.smul dx vr     (* step vector to the "right" *)
  and vus = V3.smul dy vu in  (* step vector "up" *)
  let rays: ray list = List.flatten @@ List.init nyp (fun yidx ->
    let vy = V3.add ip.ll (V3.smul (float_of_int yidx) vus) in
    List.init nxp (fun xidx ->
      let vx = V3.add vy (V3.smul (float_of_int xidx) vrs) in
      {origin=vx; direction=V3.unit @@ V3.sub vx o}
    )
  ) in
  List.to_seq rays
