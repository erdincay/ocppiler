type var =
| Var of string

type exp =
| EInt of int
| EBool of bool
| EVar of var
| ELet of var * exp * exp
| EFun of var * exp
| EIf of exp * exp * exp
| ELeq of exp * exp
| ESub of exp * exp
| EAdd of exp * exp
| EDiv of exp * exp
| ETim of exp * exp

type value =
| VInt of int
| VBool of bool
| VFun of var * exp

let rec string_of_exp (e:exp) : string =
  match e with
  | EInt n -> string_of_int n
  | EBool b -> string_of_bool b
  | EVar (Var v) -> v
  | ELet (Var v, e1, e2) -> "let " ^ v ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
  | EFun (Var v, e) -> "fun " ^ v ^ " -> " ^ string_of_exp e
  | EIf (e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3
  | ELeq (e1, e2) -> string_of_exp e1 ^ " <= " ^ string_of_exp e2
  | ESub (e1, e2) -> string_of_exp e1 ^ " - " ^ string_of_exp e2
  | EAdd (e1, e2) -> string_of_exp e1 ^ " + " ^ string_of_exp e2
  | EDiv (e1, e2) -> string_of_exp e1 ^ " / " ^ string_of_exp e2
  | ETim (e1, e2) -> string_of_exp e1 ^ " * " ^ string_of_exp e2

let string_of_value (v:value) : string =
  match v with
  | VInt n  -> string_of_int n
  | VBool b -> string_of_bool b
  | VFun (Var v, e) -> "fun " ^ v ^ " ->" ^ string_of_exp e

let rec interpret (e:exp) : value =
  match e with
  | EInt n           -> VInt n
  | EBool b          -> VBool b
  (* | EVar v           -> do something *)
  (* | ELet (Var v1, e1, e2) -> do something *)
  (* | EFun (Var v, e) -> do something *)
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
