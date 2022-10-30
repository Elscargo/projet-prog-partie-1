all:  lexeur.cmo parseur.cmo type.cmo compil.cmo aritha.ml
	ocamlc x86_64.cmo lexeur.cmo parseur.cmo type.cmo compil.cmo aritha.ml -o aritha
	pdflatex rapport.tex

lexeur.cmo: lexeur.ml
	ocamlc -c lexeur.ml

parseur.cmo: parseur.ml
	ocamlc -c parseur.ml

type.cmo: type.ml
	ocamlc -c type.ml

compil.cmo: x86_64.cmo compil.ml
	ocamlc -c compil.ml
	
x86_64.cmo: x86_64.ml
	ocamlc -c x86_64.ml				 	
