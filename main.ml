open Ast

let main ( ) =
  let file = ref "" in
  Arg.parse
    [ ]
    (fun s -> file := s)
    "Simple ML evaluator";
  let p =
    if !file != "" then
      let file = Unix.openfile !file [ Unix.O_RDONLY ] 0o644 in
      let channel = Unix.in_channel_of_descr file in
      let lexbuf = Lexing.from_channel channel in
      try Parser.prog Lexer.token lexbuf
      with e ->
        failwith (Printf.sprintf "error at line %d: %s" !Lexer.num_line
                    (Printexc.to_string e))
    else failwith "No program given" in
  Printer.p_expr stdout p;
  Printf.fprintf stdout "\n------\n";
  let _ = Type.type_expr (Type.empty_env()) p in
  Eval.print_value stdout (Eval.eval_expr (Eval.empty_env()) p);;

let _ = main ()
