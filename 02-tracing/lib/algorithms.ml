open Gg

let reflect (v: v3) (n: v3) =
  let n_u = V3.unit n in
  let d = V3.smul (2.0 *. (V3.dot v n_u)) n_u in
  V3.sub v d

let%test _ = (reflect (V3.v 1.0 (-1.0) 0.0) (V3.v 0.0 1.0 0.0)) = (V3.v 1.0 1.0 0.0)
let%test _ = V3.norm @@ V3.sub (reflect (V3.v 0.0 (-1.0) 0.0) (V3.v (0.5 *. (sqrt 2.0)) (0.5 *. (sqrt 2.0)) 0.0)) (V3.v 1.0 0.0 0.0) < 0.00001

let q_zeros a b c =
  match a with
    0.0  -> [(-1.)*.c/.b]
  | _    ->
    let delta = b*.b -. 4.*.a*.c in
    if delta < 0. then []
    else
      let sdelta = 0.5*.(sqrt delta)/.a
      and b2a = (-0.5)*.b/.a in
      [b2a -. sdelta; b2a +. sdelta]
