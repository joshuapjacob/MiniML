FILES=	ast.ml     \
	lexer.mll  \
	parser.mly \
	main.ml
CMOS=   unix.cma ast.cmo printer.cmo \
	parser.cmo lexer.cmo  \
	eval.cmo type.cmo \
	main.cmo
exec.exe: $(FILES)
	ocamlc -c ast.ml         && \
	ocamlyacc -v parser.mly  && \
	ocamllex  lexer.mll      && \
	ocamlc -c parser.mli     && \
	ocamlc -c parser.ml      && \
	ocamlc -c lexer.ml       && \
	ocamlc -c printer.mli    && \
	ocamlc -c printer.ml     && \
	ocamlc -c eval.mli		 && \
	ocamlc -c eval.ml 		 && \
	ocamlc -c type.mli		 && \
	ocamlc -c type.ml 		 && \
	ocamlc -c main.ml        && \
	ocamlc -o exec $(CMOS)
clean:
	rm -f *.cmo *.cmi *~ lexer.ml parser.ml parser.mli parser.output exec
