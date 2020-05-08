all: rpl

rpl: types.cmo parser.cmo lexer.cmo rpl.cmo
	ocamlc -o $@ $^

rpl.cmo rpl.cmi: rpl.ml lexer.cmi parser.cmi types.cmi
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
	./rpl

clean:
	rm *.cmi *.cmo rpl lexer.ml parser.ml parser.mli
