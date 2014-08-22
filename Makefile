.PHONY: all

all:
	ocamlbuild -use-ocamlfind -classic-display test.byte

clean:
	ocamlbuild -clean
	rm -f test.byte

test: all
	./test.byte
