(* Solves task "Putting it together" at end of chapter 1 (Tuples, Points and Vectors)
   and end of chapter 2 (Drawing on a Canvas) *)

(* For graphics use Vg: http://erratique.ch/software/vg *)

open Gg
open Gg.V3
open Vg

type world = {gravity: v3; wind: v3}
type projectile = {position: p3; velocity: v3}

let p_2d_pos p = V2.of_v3 p.position

type world_update = world -> projectile -> projectile
type world_stop_condition = world -> projectile -> bool
type world_print_func = world -> projectile -> unit

(** Loops the projectile [p] inside a [world] environment [w] i.e. repeatedly
    calls [update_func] until the [stop_condition] occurs.  Prints out the
    intermediate results with [print_func]. **)
let rec loop_world (w:world) (p:projectile) update_func stop_condition print_func =
  match stop_condition w p with
  | false -> (
      let pn = update_func w p in
      print_func w p;
      p :: (loop_world w pn update_func stop_condition print_func)
    )
  | true -> [p]

let update_func w p =
  let vel_move: m4 = M4.move3 p.velocity in
  {position=P3.tr vel_move p.position; velocity=p.velocity + w.gravity + w.wind}

let format_v3 v = Printf.sprintf "(%f, %f, %f)" (V3.x v) (V3.y v) (V3.z v)
let format_p3 v = Printf.sprintf "(%f, %f, %f)" (P3.x v) (P3.y v) (P3.z v)
let format_world w = Printf.sprintf "{gravity=%s; wind=%s}" (format_v3 w.gravity) (format_v3 w.wind)
let format_projectile p = Printf.sprintf "{position=%s; velocity=%s}" (format_p3 p.position) (format_v3 p.velocity)

let stop_condition _ p = (P3.y p.position) < 0.0
let print_func (w:world) (p:projectile) =
  Printf.printf "world: %s; projectile: %s\n" (format_world w) (format_projectile p)


let boundary_box pts =
  List.fold_left (fun acc pt -> Box2.add_pt acc (p_2d_pos pt)) Box2.zero pts


(* drawing *)
let dot =
  let circle = P.empty >> P.circle P2.o 0.05 in
  I.const Color.black >> I.cut circle

let gray = I.const (Color.gray 0.5)
let mark pt = dot >> I.move pt
let plot_points pts =
  let blend_mark acc pt = acc >> I.blend (mark pt) in
  List.fold_left blend_mark I.void pts

let render_points pts ?(output = "./output/world_projectile.svg") =
  let size = Size2.v 30.0 30.0 in
  (* let view = Box2.unit in *)
  let view = boundary_box pts in
  try
    let oc = open_out output in
    let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
    let img = plot_points (List.map p_2d_pos pts) in
    try
      ignore (Vgr.render r (`Image (size, view, img)));
      ignore (Vgr.render r `End);
      close_out oc;
    with e -> close_out oc; raise e
  with Sys_error e -> prerr_endline e


(* main functionality *)
let run () =
  let invec = V3.v 1.0 1.0 0.0 in
  let p = {position=P3.v 0.0 1.0 0.0; velocity=unit invec} in
  let w = {gravity=V3.v 0.0 (-0.1) 0.0; wind=V3.v (-0.01) 0.0 0.0} in

  let points = loop_world w p update_func stop_condition print_func in

  render_points points
