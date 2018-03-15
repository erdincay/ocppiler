(* Credit to: Zachary Susag (@zsusag on GitHub) for helping me figure out let/fun/fix eval & subst *)
(* LANGUAGE BASICS *)
(* e::= *)
type exp =
| EUnit                                   (* ()                    *)
| EInt    of int                          (* n                     *)
| EBool   of bool                         (* b                     *)
| EVar    of var                          (* x                     *)
| EPair   of (exp * exp)                  (* (e1, e2)              *)
| ELet    of var * typ * exp * exp        (* let x = e1 in e2      *)
| EFun    of var * typ * typ * exp        (* fun x -> e            *)
| EFix    of var * var * typ * typ * exp  (* fix f x -> e          *)
| ESeq    of exp * exp                    (* e1 ; e2               *)
| EIf     of exp * exp * exp              (* if e1 then e2 else e3 *)
| EAssign of exp * exp                    (* e1 := e2              *)
| ERef    of exp                          (* ref e                 *)
| EDeref  of exp                          (* !e                    *)
| EOp     of exp * operator * exp         (* e1 (+) e2             *)
| EApp    of exp * exp                    (* e1 e2                 *)
| EFst    of exp                          (* fst e                 *)
| ESnd    of exp                          (* snd e                 *)
| EPtr    of typ * int                    (* Ptr(n)                *)

and value =
| VUnit
| VInt  of int
| VBool of bool
| VPair of value * value
| VFun  of var * typ * typ * exp
| VFix  of var * var * typ * typ * exp

and operator =
| OLEq (* <= *)
| OSub (* -  *)
| OAdd (* +  *)
| ODiv (* /  *)
| OMul (* *  *)

and var =
| Var   of string

(* t::= *)
and typ =
| TUnit              (* ()       *)
| TInt               (* int      *)
| TBool              (* bool     *)
| TConv of typ * typ (* t1 -> t2 *)
| TPair of typ * typ (* (e1, e2) *)
| TRef  of typ       (* <t>      *)

type context = (var * typ) list
type environment = (int * value) list

let rec is_value (e:exp) : bool =
  match e with
  | EUnit | EInt _ | EBool _ | EFun _ | EFix _ | EPtr _ -> true
  | EPair (e1, e2)                                                                             -> is_value e1 && is_value e2
  | _ -> false

let context_bind (ctx:context) (vr:var) (t:typ) =
  (vr, t) :: (List.remove_assoc vr ctx) (* remove old vr in ctx list, put new in *)

let environment_assign (env:environment) (a:int) (vl:value) =
  (a, vl) :: (List.remove_assoc a env)

(* STRING GENERATION *)
let rec string_of_exp (e:exp) : string =
  match e with
  | EUnit                              -> string_of_value (VUnit)
  | EInt n                             -> string_of_value (VInt n)
  | EBool b                            -> string_of_value (VBool b)
  | EVar (Var vr)                      -> vr
  | EPair (e1, e2)                     -> "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | ELet (Var vr, t, e1, e2)           -> "(let "  ^ vr ^ " : " ^ string_of_typ t ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
  | EFun (Var vr, t1, t2, e)           -> string_of_value (VFun (Var vr, t1, t2, e))
  | EFix (Var vr1, Var vr2, t1, t2, e) -> string_of_value (VFix (Var vr1, Var vr2, t1, t2, e))
  | ESeq (e1, e2)                      -> string_of_exp e1 ^ " ; " ^ string_of_exp e2
  | EIf (e1, e2, e3)                   -> "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3 ^ ")"
  | EAssign (e1, e2)                   -> string_of_exp e1 ^ " := " ^ string_of_exp e2
  | ERef e                             -> "ref " ^ string_of_exp e
  | EDeref e                           -> "!" ^ string_of_exp e
  | EOp (e1, o, e2)                    -> "(" ^ string_of_exp e1 ^ " " ^ string_of_operator o ^ " " ^ string_of_exp e2 ^ ")"
  | EApp (e1, e2)                      -> "( " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ " )"
  | EFst e                             -> "(fst " ^ string_of_exp e ^ ")"
  | ESnd e                             -> "(fst " ^ string_of_exp e ^ ")"
  | EPtr (t, n)                        -> "Ptr(" ^ string_of_int n ^ ")"

and string_of_value (vl:value) : string =
  match vl with
  | VUnit                              -> "()"
  | VInt n                             -> string_of_int n
  | VBool b                            -> string_of_bool b
  | VPair (vl1, vl2)                   -> "(" ^ string_of_value vl1 ^ ", " ^ string_of_value vl2 ^ ")"
  | VFun (Var vr, t1, t2, e)           -> "(fun (" ^ vr ^ ":" ^ string_of_typ t1 ^ ") : " ^ string_of_typ t2 ^ " ->" ^ string_of_exp e ^ ")"
  | VFix (Var vr1, Var vr2, t1, t2, e) -> "(fix " ^ vr1 ^ " (" ^ vr2 ^ ":" ^ string_of_typ t1 ^ ") : " ^ string_of_typ t2 ^ " ->" ^ string_of_exp e ^ ")"

and string_of_operator (o:operator) : string =
  match o with
  | OLEq -> "<="
  | OSub -> "-"
  | OAdd -> "+"
  | ODiv -> "/"
  | OMul -> "*"

and string_of_typ (t:typ) : string =
  match t with
  | TUnit          -> "unit"
  | TInt           -> "int"
  | TBool          -> "bool"
  | TConv (t1, t2) -> string_of_typ t1 ^ " -> " ^ string_of_typ t2
  | TPair (t1, t2) -> string_of_typ t1 ^ " * " ^ string_of_typ t2
  | TRef t         -> "<" ^ string_of_typ t ^ ">"

(* EVALS *)
let rec eval (env:environment) (e:exp) : (env * value) list =
  match e with
  | EUnit                      -> (env, VUnit)
  | EInt n                     -> (env, VInt n)
  | EBool b                    -> (env, VBool b)
  | EPair (e1, e2)             -> (env, VPair (eval e1, eval e2))
  | ELet (vr, t, e1, e2)       -> (env, let vl = eval e1 in eval (subst vl vr e2))
  | EFun (vr, t1, t2, e)       -> (env, VFun (vr, t1, t2, e))
  | EFix (vr1, vr2, t1, t2, e) -> (env, VFix (vr1, vr2, t1, t2, e))
  | ESeq (e1, e2)              -> (env, eval e1 ; eval e2)
  | EIf (e1, e2, e3)           -> (match (eval e1, eval e2, eval e3) with
                                   | (VBool b, VInt n1, VInt n2)   -> (env, (if b then VInt n1 else VInt n2))
                                   | (VBool b, VBool b1, VBool b2) -> (env, (if b then VBool b1 else VBool b2))
                                   | _                             -> failwith "Expected matching types or boolean guard")
  | EAssign (e1, e2)           -> (* something *) (env, ())
  | ERef e                     -> (* something *) (env, ())
  | EDeref e                   -> (* something *) (env, ())
  | EOp (e1, o, e2)            -> (env, eval_op_exp e1 o e2)
  | EApp (e1, e2)              -> (match (eval e1) with
                                   | VFun (x, t1, t2, e)    -> (env, eval (subst (eval e2) x e))
                                   | VFix (f, x, t1, t2, e) -> (env, eval (subst (VFix (f, x, t1, t2, e)) f (subst (eval e2) x e)))
                                   | _                      -> failwith "Expected applicable function.")
  | EFst e                     -> (match (eval e) with
                                   | VPair (vl1, vl2) -> (env, vl1)
                                   | _                -> failwith "fst was not given pair.")
  | ESnd e                     -> (match (eval e) with
                                   | VPair (vl1, vl2) -> (env, vl2)
                                   | _                -> failwith "fst was not given pair.")
  | _                          -> failwith "Unbound variable"

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

(* SUBSTITUTION SEMANTICS *)
and subst (vl_sub:value) (vr_sub:var) (e_sub:exp) : exp =
  let subst e = subst vl_sub vr_sub e in
  match e_sub with
  | EUnit                      -> EUnit
  | EInt n                     -> EInt n
  | EBool b                    -> EBool b
  | EPair (e1, e2)             -> EPair (subst e1, subst e2)
  | EVar vr1                   -> if vr1 = vr_sub
                                  then value_to_exp vl_sub
                                  else EVar vr1
  | ELet (vr1, t, e1, e2)      -> if vr1 = vr_sub
                                  then ELet (vr1, t, subst e1, e2)
                                  else ELet (vr1, t, subst e1, subst e2)
  | EFun (vr1, t1, t2, e)      -> if vr1 = vr_sub
                                  then EFun (vr1, t1, t2, e)
                                  else EFun (vr1, t1, t2, subst e)
  | EFix (vr1, vr2, t1, t2, e) -> if vr2 = vr_sub && vr2 = vr1
                                  then EFix (vr1, vr2, t1, t2, e)
                                  else EFix (vr1, vr2, t1, t2, subst e)
  | ESeq (e1, e2)              -> ESeq (subst e1, subst e2)
  | EIf (e1, e2, e3)           -> EIf (subst e1, subst e2, subst e3)
  | EAssign (e1, e2)           -> EAssign (subst e1, subst e2)
  | ERef e                     -> ERef (subst e)
  | EDeref e                   -> EDeref (subst e)
  | EOp (e1, o, e2)            -> EOp (subst e1, o, subst e2)
  | EApp (e1, e2)              -> EApp (subst e1, subst e2)
  | EFst (e)                   -> EFst (subst e)
  | ESnd (e)                   -> ESnd (subst e)

and value_to_exp (vl:value) : exp =
  match vl with
  | VUnit                    -> EUnit
  | VInt n                   -> EInt n
  | VBool b                  -> EBool b
  | VPair (vl1, vl2)         -> EPair (value_to_exp vl1, value_to_exp vl2)
  | VFun (Var vr, t1, t2, e) -> EFun (Var vr, t1, t2, e)
  | VFix (Var vr1, Var vr2, t1, t2, e) -> EFix (Var vr1, Var vr2, t1, t2, e)

let rec typechk (e:exp) : exp =
  (let _t = typecheck [] e in e)

and typecheck (ctx:context) (e:exp) : typ =
  match e with
  | EUnit                      -> TUnit
  | EInt n                     -> TInt
  | EBool b                    -> TBool
  | EPair (e1, e2)             -> let t1 = typecheck ctx e1 in
                                  let t2 = typecheck ctx e2 in
                                  TPair (t1, t2)
  | EVar vr                    -> (try List.assoc vr ctx
                                  with Not_found -> failwith "Variable not found in context.")
  | ELet (vr1, t, e1, e2)      -> let this_ctx = context_bind ctx vr1 t in
                                  if t = typecheck ctx e1
                                  then typecheck this_ctx e2
                                  else failwith "Type mismatch in let."
  | EFun (vr, t1, t2, e)       -> let this_ctx = context_bind ctx vr t1 in
                                  if t2 = typecheck this_ctx e
                                  then TConv (t1, t2)
                                  else failwith "Type mismatch in fun."
  | EFix (vr1, vr2, t1, t2, e) -> let this_ctx = context_bind ctx vr2 t1 in
                                  let this_ctx = context_bind this_ctx vr1 (TConv (t1, t2)) in
                                  if t2 = typecheck this_ctx e
                                  then TConv (t1, t2)
                                  else failwith "Type mismatch in fix."
  | ESeq (e1, e2)              -> typecheck ctx e2
  | EIf (e1, e2, e3)           -> let t1 = typecheck ctx e1 in
                                  let t2 = typecheck ctx e2 in
                                  let t3 = typecheck ctx e3 in
                                  if t1 = TBool
                                  then if t2 = t3
                                       then t2
                                       else failwith "Type mismatch in if: e2 e3 not of same type."
                                  else failwith "Type mismatch in if: e1 was not bool"
  | EAssign (e1, e2)           -> let t1 = typecheck ctx e1 in
                                  let t2 = typecheck ctx e2 in
                                  (match t1 with
                                   | TRef t -> if t = t2
                                               then TUnit
                                               else failwith "Type mismatch in assignment: e1's type did not match e2's!"
                                   | _      -> failwith "Type mismatch in assignment: e1 was not a reference.")
  | ERef e                     -> TRef (typecheck ctx e)
  | EDeref e                   -> let t = typecheck ctx e in
                                  (match t with
                                   | TRef t -> t
                                   | _      -> failwith "Type mismatch in deref: Attempted to deref a non-reference.")
  | EOp (e1, o, e2)            -> let t1 = typecheck ctx e1 in
                                  let t2 = typecheck ctx e2 in
                                  (match (t1, t2) with
                                   | (TInt, TInt) -> TInt
                                   | _            -> failwith "Expressions were not both of type int")
  | EApp (e1, e2)              -> let t1 = typecheck ctx e1 in
                                  let t2 = typecheck ctx e2 in
                                  (match t1 with
                                   | TConv (tc1, tc2) -> if t2 = tc1
                                                         then tc2
                                                         else failwith "Type mismatch in function application"
                                   | _                -> failwith "Attempted function application with non-function.")
  | EFst e                     -> let t = typecheck ctx e in
                                  (match t with
                                   | TPair (t1, t2) -> t1
                                   | _              -> failwith "Type mismatch in fst: expected pair")
  | ESnd e                     -> let t = typecheck ctx e in
                                  (match t with
                                   | TPair (t1, t2) -> t1
                                   | _              -> failwith "Type mismatch in snd: expected pair")
  | EPtr (t, n)                -> TRef t
