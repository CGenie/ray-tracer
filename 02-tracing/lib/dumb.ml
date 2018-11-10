(* For debugging a bug in Vg
   https://github.com/dbuenzli/vg/issues/19 *)

open Gg
open Vg


let scene () =
  let boundary_box (pts) =
    List.fold_left (fun acc (pt, _) -> Box2.add_pt acc pt) Box2.zero pts in

  let dot (c: color) =
    let circle = P.empty |> P.circle P2.o 0.05 in
    I.const c |> I.cut circle in

  let mark (pt, c) = (dot c) |> I.move pt in
  let plot_points pts =
    let blend_mark acc pt = acc |> I.blend (mark pt) in
    List.fold_left blend_mark I.void pts in

  let pts = [
    (P2.v (-1.0) (-1.0), Color.red);
    (P2.v 0. 0., Color.green);
    (P2.v 1.0 1.0, Color.blue)
  ]
  and size = Size2.v 100.0 100.0 in
  let view = boundary_box pts in

  let render_svg () =
    let output = "./output/x.svg" in
    try
      let oc = open_out output in
      (* and res = 300. /. 0.0254 in  (\* 300dpi in dots per meters *\) *)
      (* let fmt = `Png (Size2.v res res) in *)
      (* let r = Vgr.create (Vgr_cairo.stored_target fmt) (`Channel oc) in *)
      let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
      let img = plot_points pts in
      try
        ignore (Vgr.render r (`Image (size, view, img)));
        ignore (Vgr.render r `End);
        close_out oc;
      with e -> close_out oc; raise e
    with Sys_error e -> prerr_endline e

  and render_png () =
    let output = "./output/x.png" in
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
   in

  render_svg ();
  render_png ()
