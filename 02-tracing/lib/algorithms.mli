open Gg

val reflect: v3 -> v3 -> v3

(**
  a x^2 + b x + c = 0  (a > 0)
  x^2 + b/a x + c/a = 0
  (x + b/2a)^2 + c/a - b^2/4a^2 = 0
  b^2/4a^2 - c/a >= 0
  Delta = (b^2 - 4ac) >= 0
  x = - b/2a + sqrt{(b^2 - 4ac)}/2a
  x = (sqrt{Delta} - b)/2a
**)

val q_zeros: float -> float -> float -> float list
