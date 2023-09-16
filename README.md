# MiniML

#### Joshua P. Jacob, Olavi Aikas

<p>In this project, we consider a (very small) subset of the ML language, and consider the evaluation and the typing of programs. Taking this project should, as a side effect, help understand better how OCaml programs actually evaluate.</p>
<h2 id="the-base-types-and-the-structure-of-the-project">The base types and the structure of the project</h2>
<p>Before starting to work, it is important that you understand well the basic data types that are supplied to you. The archive (<a href="https://www.enseignement.polytechnique.fr/informatique/CSE301/project/miniml.zip"><code>miniml.zip</code></a>, provided <a href="https://www.enseignement.polytechnique.fr/informatique/CSE301/project/miniml.zip">here</a>) contains:</p>
<ul>
<li><p>The definition of the abstract syntax tree (AST) in <code>ast.ml</code>;</p></li>
<li><p>A lexer (in <code>lexer.mll</code>) that allows to read tokens from an input stream;</p></li>
<li><p>A parser (in <code>parser.mly</code>) that can process a full program text, and produce an AST;</p></li>
<li><p>An entry point file (<code>main.ml</code>) that currently does not do much, but that you may enrich as you progress in the project;</p></li>
<li><p>A very basic <code>makefile</code>, which should either let you automate compilation completely (if you have make) or at least show you how to compile and link everything together (if you do not have make or prefer to use another build system).</p></li>
</ul>
<p>The main thing to look at first is ast.ml, as it contains the definition of the abstract syntax of programs. All your code will use these definitions, thus, please start by reading this file carefully. Definitions are documented with comments that should be self explanatory. The main thing to keep in mind is that the programs that are considered here form a small subset of the OCaml language itself, so you should be able to map each construction to the right OCaml program.</p>
<p>Essentially, the MiniML fragment supports booleans, integers, and pair types. It allows to define local names (<code>let .. in ..</code>), non-recursive and recursive functions, and basic arithmetic operations. The access to the elements of a pair is done using built-in <code>fst</code> and <code>snd</code> functions. A program typically consists of a series of declarations of constants and functions, and a final expression that produces the value of the program.</p>
<p>The lexer and parser definitions use a different syntax, but you may want to look at <code>parser.mly</code> as it simply defines a grammar. Intuitively, the syntax of programs is close to that of OCaml programs, but does not cover it in its full generality. For instance, the definition of the function that maps an integer to its successor and its binding to a name writes down <code>let f = fun (x: int) -&gt; x + 1</code>. Moreover, the <code>examples</code> directory contains a few examples, that should give you an idea of what programs look like. You are expected to add more examples whenever you consider it appropriate.</p>
<p>The <code>main.ml</code> file will be a starting point. You will need to fill it with calls to functions for printing, evaluation and typing.</p>
<h2 id="definition-of-a-pretty-printer">Definition of a pretty-printer</h2>
<p>A good way to familiarize yourself with the abstract syntax tree of a language (and with its concrete syntax too) is to write a pretty-printer, which is a function or a set of function that outputs a program. Furthermore, this pretty-printer will be used for displaying results, for error report, etc…</p>
<ul>
<li><p>Read the documentation of the <code>Printf</code> module in the OCaml standard library; it contains a series of functions which are most useful to quickly write pretty-printers. The <code>Printf.fprintf</code> function should be most useful.</p></li>
<li><p>Create a file <code>printer.ml</code> for implementation and a file <code>printer.mli</code> for interface (you will maintain consistency between both files).</p></li>
<li><p>For each type in <code>ast.ml</code>, write a pretty-printing function for it, which inputs an <code>out_channel</code>, a value of that type, and performs the output. More precisely, for a type <code>t</code>, you are expected to write a function <code>p_t: out_channel -&gt; t -&gt; unit</code>. You should try to remain close to the concrete syntax of OCaml programs.</p></li>
<li><p>Call <code>p_expr</code> from the main function.</p></li>
<li><p>Test your pretty-printer on examples.</p></li>
</ul>
<h2 id="interpreter-evaluation-of-programs">Interpreter: evaluation of programs.</h2>
<p>We now consider the evaluation of programs.</p>
<p>Before we evaluate programs, we need to define some data-types. Indeed, we will need an environment to bind names to values. Furthermore, values may be either base values (integers or booleans), pairs, or functions (defined by the list of their arguments, their body, and the environment they should evaluate in). We start with these definitions:</p>
<ul>
<li><p>Create files <code>eval.ml</code> and <code>eval.mli</code>. We leave it up to you to decide what to add in <code>eval.mli</code></p></li>
<li><p>Look at the documentation of the <code>Map</code> module, and propose a way to construct a module that defines finite domain functions from strings to any type (hint: this definition will take a handful of lines at most).</p></li>
<li><p>Define types for values and for environments.</p></li>
<li><p>Write printing functions (along the same lines as in the previous part).</p></li>
</ul>
<p>We are now ready to start the interpreter. For now, we do not consider programs with recursion.</p>
<ul>
<li><p>Write functions <code>eval_unary</code> and <code>eval_binary</code> for the evaluation of unary and binary operators (we leave the choice of their types).</p></li>
<li><p>Create a function <code>eval_expr</code> that inputs an environment and an expression, and that evaluates it.</p></li>
<li><p>Update the <code>main.ml</code> file and test your interpreter. You should add more test cases than the examples that are provided.</p></li>
</ul>
<p>More difficult question:</p>
<ul>
<li>Add support for recursion. To do this, you need to carefully think about the way programs evaluate. You may also need to slighthly revise the data-type for values.</li>
</ul>
<h2 id="typing.">Typing</h2>
<p>As you have probably noticed, it is possible to write programs that fail to evaluate due to inconsistent types. OCaml would reject such programs. An example is given in the file <code>wrong.ml</code>.</p>
<p>The typing has some similarity with the evaluation. Indeed, we also need to remember what type we give to a name, thus we still need to define a notion of typing environment.</p>
<ul>
<li><p>Create files <code>type.ml</code> and <code>type.mli</code>.</p></li>
<li><p>Define the type of typing environments.</p></li>
</ul>
<p>At first, we assume that programs do not contain recursive functions.</p>
<ul>
<li><p>Write a function <code>type_unary</code> that takes a unary operator and a type and produces either the return type or an exception if the application of the operator to a value of that type is impossible.</p></li>
<li><p>Write a function <code>type_binary</code> in a similar way.</p></li>
<li><p>Write the main typing function <code>type_expr</code>. This function should input a typing environment and an expression. It should either return its type (when the expression can be typed) or raise an exception when there is a type error. You may need to write the typing rules on paper before implementing them.</p></li>
<li><p>Call the typer from the main function, and check that it rejects all the programs that you found to cause the evaluation to crash due to type problems.</p></li>
</ul>
<p>We now consider recursion. This part is more difficult and comes as a last question, thus we do not provide very precise guidelines. Roughly speaking, recursion brings two issues to typing. First, we need to manage the typing environment differently to be consistent with the existence of recursive calls. Second, the type of the result of a recursive function may not be checkable immediately. As an example, consider <code>let rec f = (x: int) -&gt; if x &lt; 0 then -x else f (-x)</code>.</p>
<ul>
<li><p>Propose solutions to these two issues.</p></li>
<li><p>Fix your typer and test it.</p></li>
</ul>
