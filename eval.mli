type env

type value

val print_value:
  out_channel -> value -> unit

val eval_expr:
  env -> Ast.expr -> value

val empty_env:
  unit -> env