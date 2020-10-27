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

(* TypeError(name, type1, type2) means that
variable "name" has type1 but was expected to
have type2 *)
exception TypeError

let type_unary (o : uop) (t : ty) : ty =
    match o with
      Uineg -> (
          match t with
              Tint -> Tint
            | _ -> raise TypeError)
    | Ubnot -> (
        match t with
          Tbool -> Tbool
        | _ -> raise TypeError)
    | Upfst -> (
        match t with
          Tprod(t1, t2) -> t1
        | _ -> raise TypeError)
    | Upsnd -> (
        match t with
          Tprod(t1, t2) -> t2
        | _ -> raise TypeError)

let type_binary (o : bop) (t1 : ty) (t2 : ty) : ty =
    match o with
      Biadd -> (
          match t1, t2 with
            Tint, Tint -> Tint
          | _, _ -> raise TypeError)
    | Bisub -> (
          match t1, t2 with
            Tint, Tint -> Tint
          | _, _ -> raise TypeError)
    | Bimul -> (
          match t1, t2 with
            Tint, Tint -> Tint
          | _, _ -> raise TypeError)
    | Bidiv -> (
          match t1, t2 with
            Tint, Tint -> Tint
          | _, _ -> raise TypeError)
    | Bband -> (
          match t1, t2 with
            Tbool, Tbool -> Tbool
          | _, _ -> raise TypeError)
    | Bcleq -> (
          match t1, t2 with
            Tint, Tint -> Tbool
          | _, _ -> raise TypeError)
    | Bceq -> (
          match t1, t2 with
            Tint, Tint -> Tbool
          | _, _ -> raise TypeError)

let rec type_expr env e =
    match e with
      Econst(c) -> (
          match c with
            Cint n -> Tint
          | Cbool b -> Tbool )
    | Ename(name) -> (
        match Env.find_opt name env with
          None -> failwith "Variable referenced before definition!"
        | Some t -> t ) 
    | Eunary(o, e1) -> type_unary o (type_expr env e1)
    | Ebinary(o, e1, e2) -> type_binary o (type_expr env e1) (type_expr env e2)
    | Eif(ie, te, ee) -> (
        match type_expr env ie with
          Tbool -> let tte = type_expr env te in
                   let tee = type_expr env ee in
                   if tte = tee then tte else raise TypeError
        | _ -> raise TypeError )
    | Epair(e1, e2) -> Tprod(type_expr env e1, type_expr env e2)
    | Elet(r, name, be, ie) ->(
        if r then (*recursive*)
            failwith "Recursion not yet implemented"
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
                    if argtypes = List.map (type_expr env) argvals
                    then t else raise TypeError
                | _ -> failwith "Only functions can be called" )
            )
        | _ -> failwith "Only functions can be called" )
