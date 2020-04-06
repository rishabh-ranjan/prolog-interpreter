all: main

main: types.cmo parser.cmo lexer.cmo main.cmo
	ocamlc -o $@ $^

main.cmo main.cmi: main.ml lexer.cmi parser.cmi types.cmi
	ocamlc -c $<

lexer.cmo lexer.cmi: lexer.ml parser.cmi
	ocamlc -c $<

lexer.ml: lexer.mll parser.cmi
	ocamllex $<

parser.cmo: parser.ml parser.cmi types.cmi
	ocamlc -c $<

parser.cmi: parser.mli types.cmi
	ocamlc -c $<

parser.ml parser.mli: parser.mly
	ocamlyacc $<

types.cmo types.cmi: types.ml
	ocamlc -c $<

run: all
	./main

clean:
	rm *.cmi *.cmo main lexer.ml parser.ml parser.mli
