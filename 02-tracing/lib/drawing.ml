open Gg
open Vg

(* open Formatters *)
open Tracing_types

let boundary_box (pts: color_point list) =
  List.fold_left (fun acc pt -> Box2.add_pt acc (V2.of_v3 pt.point)) Box2.zero pts

let dot (c: color) =
  let circle = P.empty |> P.circle P2.o 0.05 in
  I.const c |> I.cut circle

let mark (pt: color_point) = (dot pt.color) |> I.move (V2.of_v3 pt.point)
let plot_points (pts: color_point list) =
  let blend_mark acc pt = acc |> I.blend (mark pt) in
  List.fold_left blend_mark I.void pts

let render_points ?(output = "./output/simple-scene.png") (pts: color_point list) =
  let size = Size2.v 100.0 100.0
  and view = boundary_box pts in
  (* Printf.printf "%s\n" (format_box view); *)
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
