open Ast

type type_env
type ty

val empty_env : unit -> type_env

exception TypeError of ty * ty

val type_expr : type_env -> expr -> ty