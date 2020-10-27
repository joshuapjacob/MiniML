open Ast

let rec to_string (e : expr) =
  match e with
  | Econst (const) ->
    (match const with
    | Cint (int) ->
      string_of_int int
    | Cbool (bool) ->
      string_of_bool bool)
  | Ename (name_string) ->
    name_string
  | Eunary (uop, right) ->
    let op_string =
      (match uop with
      | Uineg -> "-"
      | Ubnot -> "not "
      | Upfst -> "fst "
      | Upsnd -> "snd ") in
    "("^op_string^(to_string right)^")"
  | Ebinary (binop, left, right) ->
    let op_string =
      (match binop with
      | Biadd -> " + "
      | Bisub -> " - "
      | Bimul -> " * "
      | Bidiv -> " / "
      | Bband -> " && "
      | Bcleq -> " <= "
      | Bceq -> " = ") in
    "("^(to_string left)^op_string^(to_string right)^")"
  | Eif (if_expr, then_expr, else_expr) ->
    "if "^(to_string if_expr)^" then "^(to_string then_expr)^"\nelse "^
    (to_string else_expr)
  | Epair (first, second) ->
    "("^(to_string first)^", "^(to_string second)^")"
  | Elet (is_rec, name, expr, in_expr) ->
    let rec_string = if is_rec then "rec " else "" in
    "let "^rec_string^name^" = "^(to_string expr)^" in\n"^(to_string in_expr)
  | Efun (args, body) ->
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
    "fun "^(String.concat " " (List.map arg_to_string args))^" ->\n"^
    (to_string body)
  | Eapply (app_expr, expr_list) ->
    "("^(to_string app_expr)^" "
    ^(String.concat " " (List.map to_string expr_list))^")"

let p_expr (outchan : out_channel) (e : expr) =
  Printf.fprintf outchan "%s" (to_string e)