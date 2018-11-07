
open Gg

type phong_t = {
  ambient: float;
  diffuse: float;
  specular: float;
  shininess: float
}
type ray = {
  origin: p3;
  direction: v3
}
type shape = Ball of {
  origin: p3;
  radius: float
}
type lighting = PointLight of {
  position: p3;
  intensity: color
}
type w_object = {
  shape: shape;
  color: color;
  phong: phong_t
}
type eye = {
  origin: p3
}
type image_plane = {
  ll: p3;  (* lower-left corner of rectangle *)
  ur: p3   (* upper-right corner of rectangle *)
}
type world = {
  objects: w_object list;
  light: lighting;  (* TODO list of points *)
  background_color: color
}
type intersection = {
  normal: v3;
  point: p3;
  ray: ray;
  w_object: w_object
}
type color_point = {
  point: p3;
  color: color
}
