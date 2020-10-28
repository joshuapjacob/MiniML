open Ast

module StrOrd =
  struct
    type t = string
    let compare = String.compare
  end

module Env = Map.Make(StrOrd)

(* A new type def to allow for a constructor A
of an arbitrary type for recursive functions
whose return type is not yet known *)
type ty = A
        | Tint
        | Tbool
        | Tprod of ty * ty
        | Tarrow of ty list * ty

(*Auxiliary functions to convert old typ
types to ty types and arg lists to
(name * ty) lists *)
let rec t2t (t : typ) : ty =
    match t with
      Tint -> Tint
    | Tbool -> Tbool
    | Tprod(t1, t2) -> Tprod(t2t t1, t2t t2)
    | Tarrow(typs, t1) -> Tarrow(List.map t2t typs, t2t t1)

let upd_a (a : arg list) : (name * ty) list =
    let rec aux (wl : arg list) (cl : (name * ty) list) =
        match wl with
          [ ] -> List.rev cl
        | h :: t -> aux t ((fst h, t2t (snd h)) :: cl) in
    aux a [ ]

type type_env = ty Env.t

let empty_env ( ) = Env.empty

(* TypeError(type1, type2) means that a
variable had type1 but was expected to
have type2. This is relevant for inferring the type
of recursive functions *)
exception TypeError of ty * ty

let type_unary (o : uop) (t : ty) : ty =
    match o with
      Uineg -> (
          match t with
              Tint -> Tint
            | t1 -> raise (TypeError(t1, Tint)))
    | Ubnot -> (
        match t with
          Tbool -> Tbool
        | t1 -> raise (TypeError(t1, Tbool)))
    | Upfst -> (
        match t with
          Tprod(t1, t2) -> t1
        | t1 -> raise (TypeError(t1, Tprod(A, A))))
    | Upsnd -> (
        match t with
          Tprod(t1, t2) -> t2
        | t1 -> raise (TypeError(t1, Tprod(A, A))))

let type_binary (o : bop) (t1 : ty) (t2 : ty) : ty =
    match o with
      Biadd -> (
          match t1, t2 with
            Tint, Tint -> Tint
          | Tint, s -> raise (TypeError(s, Tint))
          | f, _ -> raise (TypeError(f, Tint)))
    | Bisub -> (
          match t1, t2 with
            Tint, Tint -> Tint
          | Tint, s -> raise (TypeError(s, Tint))
          | f, _ -> raise (TypeError(f, Tint)))
    | Bimul -> (
          match t1, t2 with
            Tint, Tint -> Tint
          | Tint, s -> raise (TypeError(s, Tint))
          | f, _ -> raise (TypeError(f, Tint)))
    | Bidiv -> (
          match t1, t2 with
            Tint, Tint -> Tint
          | Tint, s -> raise (TypeError(s, Tint))
          | f, _ -> raise (TypeError(f, Tint)))
    | Bband -> (
          match t1, t2 with
            Tbool, Tbool -> Tbool
          | Tbool, s -> raise (TypeError(s, Tbool))
          | f, _ -> raise (TypeError(f, Tbool)))
    | Bcleq -> (
          match t1, t2 with
            Tint, Tint -> Tbool
          | Tint, s -> raise (TypeError(s, Tint))
          | f, _ -> raise (TypeError(f, Tint)))
    | Bceq -> (
          match t1, t2 with
            Tint, Tint -> Tbool
          | Tint, s -> raise (TypeError(s, Tint))
          | f, _ -> raise (TypeError(f, Tint)))

let rec type_expr env e =
    match e with
      Econst(c) -> (
          match c with
            Cint n -> Tint
          | Cbool b -> Tbool )
    | Ename(name) -> (
        match Env.find_opt name env with
          None -> Printf.fprintf stdout "%s " name; failwith "Variable referenced before definition!"
        | Some t -> t )
    | Eunary(o, e1) -> type_unary o (type_expr env e1)
    | Ebinary(o, e1, e2) -> type_binary o (type_expr env e1) (type_expr env e2)
    | Eif(ie, te, ee) -> (
        match type_expr env ie with
          Tbool -> let tte = type_expr env te in
                   let tee = type_expr env ee in
                   if tte = tee then tte else raise (TypeError(tee, tte))
        | errt -> raise (TypeError(errt, Tbool)) )
    | Epair(e1, e2) -> Tprod(type_expr env e1, type_expr env e2)
    | Elet(r, name, be, ie) ->(
        if r then (*recursive*)
            match be with
              Efun(args, body) -> (
                let nargs = upd_a args in
                let nenv = Env.add name (Tarrow(List.map snd nargs, A)) env in
                let rec add_args (args : (name * ty) list) env =
                    match args with
                      [ ] -> env
                    | a :: b -> add_args b (Env.add (fst a) (snd a) env) in
                let nenv = add_args nargs nenv in
                let rec rec_typecheck exp env =
                    try (type_expr env exp, env)
                    with
                      TypeError(A, Tint) ->
                        rec_typecheck exp (Env.add name (Tarrow(List.map snd nargs, Tint)) env)
                    | TypeError(A, Tbool) ->
                        rec_typecheck exp (Env.add name (Tarrow(List.map snd nargs, Tint)) env)
                    | TypeError(_, _) ->
                        failwith "Recursion only supported for functions
                        of simple return types int or bool" in
                let _, fe = rec_typecheck body nenv in
                type_expr fe ie
                )
            | _ -> failwith "Only function expressons ca
                            be stated to be recursive"
        else type_expr (Env.add name (type_expr env be) env) ie )
    | Efun(args, e1) -> (
        let rec update_env env (args : (name * ty) list) =
            match args with
              [ ] -> env
            | arg :: rest ->
                update_env (Env.add (fst arg) (snd arg) env) rest in
        Tarrow(List.map snd (upd_a args), type_expr (update_env env (upd_a args)) e1) )
    | Eapply(ne, argvals) -> (
        match ne with
          Ename(name) -> (
            match Env.find_opt name env with
              None -> failwith "Function called before definition"
            | Some f -> (
                match f with
                  Tarrow(argtypes, t) ->
                    let rec check_args args ats =
                        match args, ats with
                          [ ], [ ] -> t
                        | a :: b, c :: d ->
                            let ta = (type_expr env a) in
                            if ta = c then
                            check_args b d
                            else raise (TypeError(ta, c))
                        | _, _ -> failwith "Too many, or not
                            enough arguments in function call" in
                    check_args argvals argtypes
                | _ -> failwith "Only functions can be called" )
            )
        | _ -> failwith "Only functions can be called" )
