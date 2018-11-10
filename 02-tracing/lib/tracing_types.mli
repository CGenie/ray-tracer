(* tracing_types.mli *)
(** Basic types for the ray tracer. *)

open Gg

type phong_t = {
  ambient: float;
  diffuse: float;
  specular: float;
  shininess: float
}
(** Describes the Phong material properites of an object. *)

type ray = {
  origin: p3;
  direction: v3
}
(** Describes single ray shot from camera's eye. *)

type shape = Ball of {
  origin: p3;
  radius: float
} | Affine of {
  m: m4;
  inv_m: m4;  (* For performance reasons, so that I don't have to invert it all the time *)
  shape: shape
}

(** Basic shapes that we know how to render.

    [Ball{origin; radius}] A ball of origin [origin] and radius [radius].

    [Affine{m; inv_m; shape}] Affine transformation with matrix [m] (inverse is [inv_m])
    of shape [shape]:
      - {{:http://www.unknownroad.com/rtfm/graphics/rt_normals.html}http://www.unknownroad.com/rtfm/graphics/rt_normals.html}
      - {{:http://erratique.ch/software/gg/doc/Gg.V3.html#VALtr}http://erratique.ch/software/gg/doc/Gg.V3.html#VALtr}
      - {{:http://erratique.ch/software/gg/doc/Gg.P3.html#VALtr}http://erratique.ch/software/gg/doc/Gg.P3.html#VALtr}
 *)

type lighting = PointLight of {
  position: p3;
  intensity: color
}

(** Describes ligting source.

    [PointLight{position; intensity}] A point light positioned at [position] with intensity color [intensity]. *)

type w_object = {
  shape: shape;
  color: color;
  phong: phong_t
}

(** An renderable object with color and Phong properties attached. *)

type eye = p3

(** Camera's eye. *)

type image_plane = {
  ll: p3;  (** lower-left corner of rectangle *)
  ur: p3   (** upper-right corner of rectangle *)
}

(** Camera image plane. *)
type camera = {
  eye: eye;
  direction: v3;
  distance: float;
  width: float;
  height: float
}

(** [camera] type.
    Parameters are:
    [eye] position,
    [direction] of look (a normal vector),
    image plane [distance] from eye,
    image plane [width],
    image plane [height].

{[
width x height
 ------^------ image plane
       |
       |direction
       |
      eye
]}

*)

type world = {
  objects: w_object list;
  lights: lighting list;
  background_color: color
}

(** Describes all physical objects of a scene. *)

type intersection = {
  normal: v3;
  point: p3;
  ray: ray;
  w_object: w_object
}

(** Type to hold [ray] - [w_object] intersection data. *)

type color_point = {
  point: p3;
  color: color
}

(** A point with color attached. *)
