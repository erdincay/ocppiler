type value =
| VInt of int
| VBool of bool

type exp =
| EInt of int
| EBool of bool
| EIf of exp * exp * exp
| ELeq of exp * exp
| ESub of exp * exp
| EAdd of exp * exp
| EDiv of exp * exp
| ETim of exp * exp

let string_of_value (v:value) : string =
  match v with
  | VInt n  -> string_of_int n
  | VBool b -> string_of_bool b

let rec interpret (e:exp) : value =
  match e with
  | EInt n           -> VInt n
  | EBool b          -> VBool b
  | EIf (e1, e2, e3) -> (match (interpret e1, interpret e2, interpret e3) with
                         | (VBool b, VInt n1, VInt n2)   -> (if b then VInt n1 else VInt n2)
                         | (VBool b, VBool b1, VBool b2) -> (if b then VBool b1 else VBool b2)
                         | _                             -> failwith "Expected matching types or boolean guard")
  | ELeq (e1, e2)    -> (match (interpret e1, interpret e2) with
                         | (VInt n1, VInt n2) -> VBool (n1 <= n2)
                         | _                  -> failwith "Expected int, given non-int")
  | ESub (e1, e2)    -> (match (interpret e1, interpret e2) with
                         | (VInt n1, VInt n2) -> VInt (n1 - n2)
                         | _                  -> failwith "Expected int, given non-int")
  | EAdd (e1, e2)    -> (match (interpret e1, interpret e2) with
                         | (VInt n1, VInt n2) -> VInt (n1 + n2)
                         | _                  -> failwith "Expected int, given non-int")
  | EDiv (e1, e2)    -> (match (interpret e1, interpret e2) with
                         | (VInt n1, VInt n2) -> if n2 = 0
                                                 then failwith "Attempt to divide by zero"
                                                 else VInt (n1 / n2)
                         | _                  -> failwith "Expected int, given non-int")
  | ETim (e1, e2)    -> (match (interpret e1, interpret e2) with
                         | (VInt n1, VInt n2) -> VInt (n1 * n2)
                         | _                  -> failwith "Expected int, given non-int")
