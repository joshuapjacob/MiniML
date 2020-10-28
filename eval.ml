open Ast

module StrOrd =
  struct
    type t = string
    let compare = String.compare
  end

module Env = Map.Make(StrOrd)

type value =
  | Vbool of bool
  | Vint of int
  | Vpair of value * value
  | Vfunc of (arg list * expr * env ref)
  and env = value Env.t

let rec to_string (v:value) =
  match v with
  | Vbool (b) -> string_of_bool b
  | Vint (i) -> string_of_int i
  | Vpair (v1,v2) -> "("^(to_string v1)^", "^(to_string v2)^")"
  | Vfunc (args, expr, env) ->
    let rec typ_to_string (t: typ) =
      match t with
      | Tint ->
        "int"
      | Tbool ->
        "bool"
      | Tprod (ft,st) ->
        (typ_to_string ft)^" * "^(typ_to_string st)
      | Tarrow (t_list, return_t) ->
        (String.concat " -> " (List.map typ_to_string t_list))^" -> "^
        (typ_to_string return_t) in
    let arg_to_string arg = "("^(fst arg)^": "^(typ_to_string (snd arg))^")" in
    "fun "^(String.concat " " (List.map arg_to_string args))

let print_value (outchan:out_channel) (v:value) =
  Printf.fprintf outchan "%s" (to_string v)

let eval_unary (u:uop) (v:value): value =
  match v with
  | Vint (i) ->
    (match u with
    | Uineg -> Vint (-i)
    | _     -> failwith "Illegal Unary Integer Operation")
  | Vbool (b) ->
    (match u with
    | Ubnot -> Vbool (not b)
    | _     -> failwith "Illegal Unary Boolean Operation")
  | Vpair (v1,v2) ->
    (match u with
    | Upfst -> v1
    | Upsnd -> v2
    | _     -> failwith "Illegal Unary Pair Operation")
  | _ -> failwith "Unary Operation Type Mismatch"

let eval_binary (b:bop) (v1:value) (v2:value): value =
  match v1,v2 with
  | Vint (i1), Vint (i2) ->
    (match b with
    | Biadd -> Vint  (i1 +  i2)
    | Bisub -> Vint  (i1 -  i2)
    | Bimul -> Vint  (i1 *  i2)
    | Bidiv -> Vint  (i1 /  i2)
    | Bcleq -> Vbool (i1 <= i2)
    | Bceq  -> Vbool (i1 =  i2)
    | _     -> failwith "Illegal Binary Integer Operation")
  | Vbool (b1), Vbool (b2) ->
    (match b with
    | Bband -> Vbool (b1 && b2)
    | _     -> failwith "Illegal Binary Boolean Operation")
  | _,_ -> failwith "Binary Operation Type Mismatch"

let rec eval_expr (env:env) (e:expr): value =
  match e with
  | Econst (const) ->
    (match const with
    | Cint i -> Vint i
    | Cbool b -> Vbool b)
  | Ename (name) -> Env.find name env
  | Eunary (uop,v) -> eval_unary uop (eval_expr env v)
  | Ebinary (bop,v1,v2) -> eval_binary bop (eval_expr env v1) (eval_expr env v2)
  | Eif (if_expr, then_expr, else_expr) ->
    if (eval_expr env if_expr) = Vbool true
    then (eval_expr env then_expr)
    else (eval_expr env else_expr)
  | Epair (first,second) -> Vpair ((eval_expr env first),(eval_expr env second))
  | Elet (is_rec, name, expr, in_expr) ->
    if is_rec then
      match expr with
      | Efun (args,body) ->
        let new_env = ref env in
        let func_val = Vfunc(args, body, new_env) in
        let env_with_fn = (Env.add name func_val env) in
        new_env := env_with_fn;
        eval_expr env_with_fn in_expr
      | _ -> failwith "Bad Recursion"
    else
      eval_expr (Env.add name (eval_expr env expr) env) in_expr
  | Efun (args, body) -> Vfunc (args, body, ref env)
  | Eapply (app_expr, expr_list) ->
    let arg_values = (List.map (eval_expr env) expr_list) in
    let func = (eval_expr env app_expr) in
    match func with
    | Vfunc (f_args, f_expr, env) ->
      let rec get_new_env i : env =
        if i = 0 then
          Env.add
            (fst (List.nth f_args i))
            (List.nth arg_values i)
            env.contents
        else
          Env.add
            (fst (List.nth f_args i))
            (List.nth arg_values i)
            (get_new_env (i-1)) in
      let new_env = get_new_env (List.length arg_values - 1) in
      eval_expr new_env f_expr
    | _ -> failwith "Not A Function"

let empty_env () = Env.empty