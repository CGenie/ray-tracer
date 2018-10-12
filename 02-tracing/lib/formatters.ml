open Tracing_types

let format_v2 v = Printf.sprintf "(%f, %f)" (Gg.V2.x v) (Gg.V2.y v)
let format_v3 v = Printf.sprintf "(%f, %f, %f)" (Gg.V3.x v) (Gg.V3.y v) (Gg.V3.z v)
let format_p2 v = Printf.sprintf "(%f, %f)" (Gg.P2.x v) (Gg.P2.y v)
let format_p3 v = Printf.sprintf "(%f, %f, %f)" (Gg.P3.x v) (Gg.P3.y v) (Gg.P3.z v)
let format_ray (r: ray) = Printf.sprintf "ray{origin=%s; direction=%s}" (format_p3 r.origin) (format_v3 r.direction)
let format_box b = Printf.sprintf "Box2{origin=%s; size=%s}" (format_p2 @@ Gg.Box2.o b) (format_v2 @@ Gg.Box2.size b)
