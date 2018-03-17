(* Credit to: Zachary Susag (@zsusag on GitHub) for helping me figure out let/fun/fix eval and subst, and if evaluation *)
(* Also thumbs up to Reilly Noonan Grant, whose code I peeked at on occasion to get myself up to speed with Menhir's structure *)
(* LANGUAGE BASICS *)
(* e::= *)
type exp =
| EUnit                                   (* ()                    *)
| EInt    of int                          (* n                     *)
| EBool   of bool                         (* b                     *)
| EVar    of var                          (* x                     *)
| ETuple  of (exp list) * int             (* (e1, e2)              *)
| EPtr    of typ * int                    (* Ptr(n)                *)
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
| EGet    of int * exp                    (* get e1, e2...         *)
| EWhile  of exp * exp                    (* while e1 do e2 end    *)

and value =
| VUnit
| VInt   of int
| VBool  of bool
| VTuple of (value list) * int
| VPtr   of typ * int
| VFun   of var * typ * typ * exp
| VFix   of var * var * typ * typ * exp

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
| TUnit                      (* ()       *)
| TInt                       (* int      *)
| TBool                      (* bool     *)
| TConv of typ * typ         (* t1 -> t2 *)
| TTuple of (typ list) * int (* (e1, e2) *)
| TRef  of typ               (* <t>      *)

type context = (var * typ) list
type environment = (int * value) list

(* EXTERNALS *)
let context_bind (ctx:context) (vr:var) (t:typ) =
  (vr, t) :: (List.remove_assoc vr ctx) (* remove old vr in ctx list, put new in *)

let environment_assign (env:environment) (a:int) (vl:value) : environment =
  (a, vl) :: (List.remove_assoc a env)

let cur_addr = ref (0)

let gen_addr () =
  cur_addr := !cur_addr + 1;
  !cur_addr

let gen_ptr (env:environment) (t:typ) (vl:value) =
  let a = gen_addr () in
  (environment_assign env a vl, VPtr (t, a))

let rec get_n n vls =
  match (n, vls) with
  | (0, vl::_)  -> vl
  | (n, vl::vls) -> get_n (n-1) vls
  | (n, _)     -> failwith "Tuple doesn't have that many values"

(* STRINGS *)
let rec string_of_exp (e:exp) : string =
  match e with
  | EUnit                              -> string_of_value (VUnit)
  | EInt n                             -> string_of_value (VInt n)
  | EBool b                            -> string_of_value (VBool b)
  | EVar (Var vr)                      -> vr
  | ETuple (exps, n)                   -> let comma_between ex = (string_of_exp ex) ^ ", " in
                                          "(" ^ (List.fold_left (^) "" (List.map comma_between exps)) ^ ")"
  | EPtr (t, a)                        -> string_of_value (VPtr (t, a))
  | ELet (Var vr, t, e1, e2)           -> "(let "  ^ vr ^ " : " ^ string_of_typ t ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
  | EFun (Var vr, t1, t2, e)           -> string_of_value (VFun (Var vr, t1, t2, e))
  | EFix (Var vr1, Var vr2, t1, t2, e) -> string_of_value (VFix (Var vr1, Var vr2, t1, t2, e))
  | ESeq (e1, e2)                      -> string_of_exp e1 ^ " ; " ^ string_of_exp e2
  | EIf (e1, e2, e3)                   -> "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3 ^ ")"
  | EAssign (e1, e2)                   -> string_of_exp e1 ^ " := " ^ string_of_exp e2
  | ERef e                             -> "ref " ^ string_of_exp e
  | EDeref e                           -> "!" ^ string_of_exp e
  | EOp (e1, o, e2)                    -> "(" ^ string_of_exp e1 ^ " " ^ string_of_operator o ^ " " ^ string_of_exp e2 ^ ")"
  | EApp (e1, e2)                      -> "( " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ " )"
  | EGet (n, e)                        -> "(get " ^ string_of_int n ^ " " ^ string_of_exp e ^ ")"
  | EWhile (e1, e2)                    -> "(while " ^ string_of_exp e1 ^ " do " ^ string_of_exp e2 ^ " end)"

and string_of_value (vl:value) : string =
  match vl with
  | VUnit                              -> "()"
  | VInt n                             -> string_of_int n
  | VBool b                            -> string_of_bool b
  | VTuple (vls, n)                    -> let comma_between v = (string_of_value v) ^ ", " in
                                          "(" ^ (List.fold_left (^) "" (List.map comma_between vls)) ^ ")"
  | VFun (Var vr, t1, t2, e)           -> "(fun (" ^ vr ^ ":" ^ string_of_typ t1 ^ ") : " ^ string_of_typ t2 ^ " -> " ^ string_of_exp e ^ ")"
  | VFix (Var vr1, Var vr2, t1, t2, e) -> "(fix " ^ vr1 ^ " (" ^ vr2 ^ ":" ^ string_of_typ t1 ^ ") : " ^ string_of_typ t2 ^ " ->" ^ string_of_exp e ^ ")"
  | VPtr (t, n)                        -> "Ptr(" ^ string_of_int n ^ ")"

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
  | TTuple (ts, n) -> let star_between ty = (string_of_typ ty) ^ ", " in
                      "(" ^ (List.fold_left (^) "" (List.map star_between ts)) ^ ")"
  | TRef t         -> "<" ^ string_of_typ t ^ ">"

(* EVALUATION *)
let rec interpret (e:exp) : value =
  snd (eval [] e)

and eval (env:environment) (e:exp) : (environment * value) =
  (* huge thanks to Theo Kalfas for this pattern that saved my environment *)
  let envr = ref env in
  let evalr e =
  let (saved_env, val_only_eval) = eval !envr e in
  envr := saved_env; val_only_eval in
  let val_only_eval =
  match e with
  | EUnit                      -> VUnit
  | EInt n                     -> VInt n
  | EBool b                    -> VBool b
  | ETuple (e, n)              -> VTuple (List.map evalr e, n)
  | ELet (vr, t, e1, e2)       -> let vl = evalr e1 in evalr (subst vl vr e2)
  | EFun (vr, t1, t2, e)       -> VFun (vr, t1, t2, e)
  | EFix (vr1, vr2, t1, t2, e) -> VFix (vr1, vr2, t1, t2, e)
  | ESeq (e1, e2)              -> (evalr e1 ; evalr e2)
  | EIf (e1, e2, e3)           -> (match (evalr e1) with
                                   | VBool true  -> evalr e2
                                   | VBool false -> evalr e3
                                   | _           -> failwith "EIf expected a boolean e1.")
  | EAssign (e1, e2)           -> (match (evalr e1, evalr e2) with
                                   | (VPtr (t, a), exp2) -> (environment_assign !envr a exp2); VUnit
                                   | _                   -> failwith "EAssign expected a ref and a value.")
  | ERef e                     -> let (gen_env, ptr) = gen_ptr !envr (typecheck [] e) (evalr e) in
                                  envr := gen_env; ptr
  | EDeref e                   -> (match (evalr e) with
                                   | VPtr (t, a) -> List.assoc a !envr
                                   | _           -> failwith "Expected to deref a pointer.")
  | EOp (e1, o, e2)            -> snd (eval_op_exp !envr e1 o e2)
  | EApp (e1, e2)              -> (match (evalr e1) with
                                   | VFun (x, t1, t2, e)    -> evalr (subst (evalr e2) x e)
                                   | VFix (f, x, t1, t2, e) -> evalr (subst (VFix (f, x, t1, t2, e)) f (subst (evalr e2) x e))
                                   | _                      -> failwith "Expected applicable function.")
  | EGet (ng, e)              -> (match (evalr e) with
                                   | VTuple (vls, nt) -> if ng < nt
                                                         then get_n ng vls
                                                         else failwith "Index out of bounds in tuple."
                                   | _                -> failwith "get was not given tuple.")
  | EWhile (e1, e2)            -> (evalr (EIf (e1, ESeq (e2, EWhile (e1, e2)), EUnit)))
  | EPtr (t, a)                -> VPtr (t, a)
  | _                          -> failwith ("Unbound variable " ^ string_of_exp e)
  in
  (!envr, val_only_eval)

and eval_op_exp (env:environment) (e1:exp) (o:operator) (e2:exp) : environment * value =
let eval e = snd (eval env e) in
  match o with
  | OLEq -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> (env, VBool (n1 <= n2))
             | _                  -> failwith "Expected integer arguments for less-than-or-equals-to operator.")
  | OSub -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> (env, VInt (n1 - n2))
             | _                  -> failwith "Expected integer arguments for subtraction operator.")
  | OAdd -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> (env, VInt (n1 + n2))
             | _                  -> failwith "Expected integer arguments for addition operator.")
  | ODiv -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> if n2 = 0
                                     then failwith "Expected non-zero denominator for division operator."
                                     else (env, VInt (n1 / n2))
             | _                  -> failwith "Expected integer arguments for division operator.")
  | OMul -> (match (eval e1, eval e2) with
             | (VInt n1, VInt n2) -> (env, VInt (n1 * n2))
             | _                  -> failwith "Expected integer arguments for multiplication operator.")

(* SUBSTITUTION *)
and subst (vl_sub:value) (vr_sub:var) (e_sub:exp) : exp =
  let subst e = subst vl_sub vr_sub e in
  match e_sub with
  | EUnit                      -> EUnit
  | EInt n                     -> EInt n
  | EBool b                    -> EBool b
  | ETuple (exps, n)           -> ETuple (List.map subst exps, n)
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
  | EGet (n, e)                -> EGet (n, subst e)
  | EPtr (t, a)                -> EPtr (t, a)
  | EWhile (e1, e2)            -> EWhile (subst e1, subst e2)

and value_to_exp (vl:value) : exp =
  match vl with
  | VUnit                              -> EUnit
  | VInt n                             -> EInt n
  | VBool b                            -> EBool b
  | VTuple (vls, n)                    -> ETuple (List.map value_to_exp vls, n)
  | VFun (Var vr, t1, t2, e)           -> EFun (Var vr, t1, t2, e)
  | VFix (Var vr1, Var vr2, t1, t2, e) -> EFix (Var vr1, Var vr2, t1, t2, e)
  | VPtr (t, a)                        -> EPtr (t, a)

(* TYPECHECKING *)
and typechk (e:exp) : exp =
  (let _t = typecheck [] e in e)

and typecheck (ctx:context) (e:exp) : typ =
  match e with
  | EUnit                      -> TUnit
  | EInt n                     -> TInt
  | EBool b                    -> TBool
  | ETuple (exps, n)           -> TTuple ((List.map (typecheck ctx) exps), n)
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
                                               else failwith "Type mismatch in assignment: e1's type did not match e2's"
                                   | _      -> failwith "Type mismatch in assignment: e1 was not a reference.")
  | ERef e                     -> TRef (typecheck ctx e)
  | EDeref e                   -> let t = typecheck ctx e in
                                  (match t with
                                   | TRef t -> t
                                   | _      -> failwith "Type mismatch in deref: Attempted to deref a non-reference.")
  | EOp (e1, o, e2)            -> let t1 = typecheck ctx e1 in
                                  let t2 = typecheck ctx e2 in
                                  (match (t1, t2) with
                                   | (TInt, TInt) -> (match o with
                                                      | OLEq -> TBool
                                                      | _    -> TInt)
                                   | _            -> failwith "Type mismatch in operator exp: Expressions were not both of type int")
  | EApp (e1, e2)              -> let t1 = typecheck ctx e1 in
                                  let t2 = typecheck ctx e2 in
                                  (match t1 with
                                   | TConv (tc1, tc2) -> if t2 = tc1
                                                         then tc2
                                                         else failwith "Type mismatch in function application"
                                   | _                -> failwith "Type mismatch in function application: Expected e1 to be function.")
  | EGet (ng, e)                -> (match (typecheck ctx e) with
                                    | TTuple (ts, nt) -> if ng < nt
                                                         then List.nth ts ng
                                                         else failwith "Index out of bounds in ttuple."
                                    | _                -> failwith "get was not given tuple.")
  | EPtr (t, a)                -> TRef t
  | EWhile (e1, e2)            -> let t1 = typecheck ctx e1 in
                                  match t1 with
                                  | TBool -> TUnit
                                  | _     -> failwith "Type mismatch in while: Expected boolean e1."
