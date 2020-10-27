(*The typing has some similarity with the evaluation. Indeed, we also need to remember what type we give to a name, thus we still need to define a notion of typing environment.

Create files type.ml and type.mli.

Define the type of typing environments.

At first, we assume that programs do not contain recursive functions.

Write a function type_unary that takes a unary operator and a type and produces either the return type or an exception if the application of the operator to a value of that type is impossible.

Write a function type_binary in a similar way.

Write the main typing function type_expr. This function should input a typing environment and an expression. It should either return its type (when the expression can be typed) or raise an exception when there is a type error. You may need to write the typing rules on paper before implementing them.

Call the typer from the main function, and check that it rejects all the programs that you found to cause the evaluation to crash due to type problems.

We now consider recursion. This part is more difficult and comes as a last question, thus we do not provide very precise guidelines. Roughly speaking, recursion brings two issues to typing. First, we need to manage the typing environment differently to be consistent with the existence of recursive calls. Second, the type of the result of a recursive function may not be checkable immediately. As an example, consider let rec f = (x: int) -> if x < 0 then -x else f (-x).

Propose solutions to these two issues.

Fix your typer and test it.*)

open Ast

type type_env
type ty

val empty_env : unit -> type_env

exception TypeError

val type_expr : type_env -> expr -> ty