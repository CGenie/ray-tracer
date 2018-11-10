(* drawing.ml *)
(** Drawing functions. *)

open Gg
open Vg

open Tracing_types

let boundary_box pts =
  Seq.fold_left (fun acc pt -> Box2.add_pt acc (V2.of_v3 pt.point)) Box2.zero pts
(** [boundary_box pts] Determine the smallest box bounding points [pts]. *)

let dot (c: color) =
  let circle = P.empty |> P.circle P2.o 0.05 in
  I.const c |> I.cut circle
(** [dot c] Draws a single dot in [vg] of color [c]. *)

let mark (pt: color_point) = (dot pt.color) |> I.move (V2.of_v3 pt.point)
(** [mark pt] Mark [pt] of type {{!Tracing__.Tracing_types.color_point}color_point} *)

let plot_points pts =
  let blend_mark acc pt = acc |> I.blend (mark pt) in
  Seq.fold_left blend_mark I.void pts
(** [plot_points pts] plots points [pts] in [vg]. *)

let render_points ?(output = "./output/simple-scene.png") pts =
  let size = Size2.v 100.0 100.0
  and view = boundary_box pts in
  try
    let oc = open_out output
    and res = 300. /. 0.0254 in  (* 300dpi in dots per meters *)
    let fmt = `Png (Size2.v res res) in
    let r = Vgr.create (Vgr_cairo.stored_target fmt) (`Channel oc) in
    let img = plot_points pts in
    try
      ignore (Vgr.render r (`Image (size, view, img)));
      ignore (Vgr.render r `End);
      close_out oc;
    with e -> close_out oc; raise e
  with Sys_error e -> prerr_endline e
(** [render_points ~output:string pts] Renders points [pts] onto an output image path. *)
