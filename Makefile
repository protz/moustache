.PHONY: all

all:
	ocamlbuild -use-ocamlfind -classic-display test.byte

clean:
	ocamlbuild -clean
	rm -f test.byte

test: all
	./test.byte

export:
	git archive --format tar --prefix streaml/ HEAD | bzip2 -9 > ../streaml.tar.bz2
