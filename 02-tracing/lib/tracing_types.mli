
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
} | Affine of {
  m: m4;
  inv_m: m4;  (* For performance reasons, so that I don't have to invert it all the time *)
  shape: shape
}  (** Affine transformation:
       http://www.unknownroad.com/rtfm/graphics/rt_normals.html
       http://erratique.ch/software/gg/doc/Gg.V3.html#VALtr
       http://erratique.ch/software/gg/doc/Gg.P3.html#VALtr **)
type lighting = PointLight of {
  position: p3;
  intensity: color
}
type w_object = {
  shape: shape;
  color: color;
  phong: phong_t
}
type eye = p3
type image_plane = {
  ll: p3;  (* lower-left corner of rectangle *)
  ur: p3   (* upper-right corner of rectangle *)
}
(** camera type.
    Parameters are:
    eye position,
    direction of look (a normal vector),
    image plane distance from eye,
    image plane width,
    image plane height.

width x height
 ------^------ image plane
       |
      d|
       |
      eye

 **)
type camera = {
  eye: eye;
  direction: v3;
  distance: float;
  width: float;
  height: float
}
type world = {
  objects: w_object list;
  lights: lighting list;
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
