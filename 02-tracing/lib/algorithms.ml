open Gg

(** Middle point between 2 points *)
let mid_pt (a: p3) (b: p3) = V3.smul 0.5 (V3.add a b)

(** Reflect a vector around a normal **)
let reflect (v: v3) (n: v3) =
  let n_u = V3.unit n in
  let d = V3.smul (2.0 *. (V3.dot v n_u)) n_u in
  V3.sub v d

let%test _ = (reflect (V3.v 1.0 (-1.0) 0.0) (V3.v 0.0 1.0 0.0)) = (V3.v 1.0 1.0 0.0)
let%test _ = V3.norm @@ V3.sub (reflect (V3.v 0.0 (-1.0) 0.0) (V3.v (0.5 *. (sqrt 2.0)) (0.5 *. (sqrt 2.0)) 0.0)) (V3.v 1.0 0.0 0.0) < 0.00001
