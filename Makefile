OB = ocamlbuild -use-ocamlfind

all:
	$(OB) cats.cma
	$(OB) cats.cmxa

clean:
	$(OB) -clean

.PHONE: all clean
