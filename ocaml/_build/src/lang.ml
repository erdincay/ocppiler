type exp =
| ETim of exp * exp
| EDiv of exp * exp
| EAdd of exp * exp
| ESub of exp * exp
| ELeq of exp * exp
| EInt of int

let rec interpret (e:exp) : int =
  match e with
  | ETim (e1, e2) -> interpret e1 * interpret e2
  | EDiv (e1, e2) -> interpret e1 / interpret e2
  | EAdd (e1, e2) -> interpret e1 + interpret e2
  | ESub (e1, e2) -> interpret e1 - interpret e2
  (*| ELeq (e1, e2) -> interpret e1 <= interpret e2*)
  | EInt n        -> n
