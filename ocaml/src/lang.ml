(* TYPES *)
type exp =
| EInt  of int
| EBool of bool
| EVar  of var
| ELet  of var * exp * exp
| EFun  of var * exp
| EFix  of var * var * exp
| EIf   of exp * exp * exp
| EOp   of exp * operator * exp
| EApp  of exp * exp

and value =
| VInt  of int
| VBool of bool
| VFun  of var * exp
| VFix  of var * var * exp

and operator =
| OLEq
| OSub
| OAdd
| ODiv
| OMul

and var =
| Var   of string

(* STRING GENERATION *)
let rec string_of_exp (e:exp) : string =
  match e with
  | EInt n                     -> string_of_value (VInt n)
  | EBool b                    -> string_of_value (VBool b)
  | EVar (Var vr)              -> vr
  | ELet (Var vr, e1, e2)      -> "(let "  ^ vr ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
  | EFun (Var vr, e)           -> string_of_value (VFun (Var vr, e))
  | EFix (Var vr1, Var vr2, e) -> string_of_value (VFix (Var vr1, Var vr2, e))
  | EIf (e1, e2, e3)           -> "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3 ^ ")"
  | EOp (e1, o, e2)            -> "(" ^ string_of_exp e1 ^ " " ^ string_of_operator o ^ " " ^ string_of_exp e2 ^ ")"
  | EApp (e1, e2)              -> "( " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ " )"

and string_of_value (vl:value) : string =
  match vl with
  | VInt n                     -> string_of_int n
  | VBool b                    -> string_of_bool b
  | VFun (Var vr, e)           -> "(fun " ^ vr ^ " ->" ^ string_of_exp e ^ ")"
  | VFix (Var vr1, Var vr2, e) -> "(fix " ^ vr1 ^ " " ^ vr2 ^ " -> " ^ string_of_exp e ^ ")"

and string_of_operator (o:operator) : string =
  match o with
  | OLEq -> "<="
  | OSub -> "-"
  | OAdd -> "+"
  | ODiv -> "/"
  | OMul -> "*"

(* SUBSTITUTION SEMANTICS *)
let rec subst (vl_sub:value) (vr_sub:var) (e_sub:exp) : exp =
  match e_sub with
  | EInt n             -> EInt n
  | EBool b            -> EBool b
  | EVar vr1           -> if vr1 = vr_sub
                          then (match vl_sub with
                                | VInt n                     -> EInt n
                                | VBool b                    -> EBool b
                                | VFun (Var vr, e)           -> EFun (Var vr, e)
                                | VFix (Var vr1, Var vr2, e) -> EFix (Var vr1, Var vr2, e))
                          else EVar vr1
  | ELet (vr1, e1, e2) -> if vr1 = vr_sub
                          then ELet (vr1, subst vl_sub vr_sub e1, e2)
                          else ELet (vr1, subst vl_sub vr_sub e1, subst vl_sub vr_sub e2)
  | EFun (vr1, e)      -> if vr1 = vr_sub
                          then EFun (vr1, e)
                          else EFun (vr1, subst vl_sub vr_sub e)
  | EFix (vr1, vr2, e) -> EFix (vr1, vr2, e)
  | EIf (e1, e2, e3)   -> EIf (subst vl_sub vr_sub e1, subst vl_sub vr_sub e2, subst vl_sub vr_sub e3)
  | EOp (e1, o, e2)    -> EOp (subst vl_sub vr_sub e1, o, subst vl_sub vr_sub e2)
  | EApp (e1, e2)      -> EApp (subst vl_sub vr_sub e1, subst vl_sub vr_sub e2)

(* EVALS *)
let rec eval (e:exp) : value =
  match e with
  | EInt n               -> VInt n
  | EBool b              -> VBool b
  | EFun (vr, e)         -> VFun (vr, e)
  | EFix (vr1, vr2, e)   -> VFix (vr1, vr2, e)
  | ELet (vr, e1, e2)    -> let vl = eval e1 in eval (subst vl vr e2)
  | EIf (e1, e2, e3)     -> (match (eval e1, eval e2, eval e3) with
                             | (VBool b, VInt n1, VInt n2)   -> (if b then VInt n1 else VInt n2)
                             | (VBool b, VBool b1, VBool b2) -> (if b then VBool b1 else VBool b2)
                             | _                             -> failwith "Expected matching types or boolean guard")
  | EOp (e1, o, e2)      -> eval_op_exp e1 o e2
  | EApp (e1, e2)        -> (match (eval e1) with
                             | VFun (x, e)    -> eval (subst  (eval e2) x e)
                             | VFix (f, x, e) -> eval (subst (VFix (f, x, e)) f (subst (eval e2) x e))
                             | _              -> failwith "Expected applicable function.")
  | _                    -> failwith "Unbound variable"

and eval_op_exp (e1:exp) (o:operator) (e2:exp) : value =
  match o with
  | OLEq -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> VBool (n1 <= n2)
             | _                  -> failwith "Expected integer arguments for less-than-or-equals-to operator.")
  | OSub -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> VInt (n1 - n2)
             | _                  -> failwith "Expected integer arguments for subtraction operator.")
  | OAdd -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> VInt (n1 + n2)
             | _                  -> failwith "Expected integer arguments for addition operator.")
  | ODiv -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> if n2 = 0
                                     then failwith "Expected non-zero denominator for division operator."
                                     else VInt (n1 / n2)
             | _                  -> failwith "Expected integer arguments for division operator.")
  | OMul -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> VInt (n1 * n2)
             | _                  -> failwith "Expected integer arguments for multiplication operator.")
