.PHONY: all
all: Language/CIL.hs

test: Test.hs Language/CIL.hs
	ghc --make -W -fglasgow-exts -o test Test.hs

ciltypes: CILTypes.hs
	ghc --make -W -fglasgow-exts -o ciltypes CILTypes.hs

CIL.hs dumpcil/dump_cil.ml: ciltypes
	./ciltypes

Language/CIL.hs: CIL.hs
	cp CIL.hs Language/CIL.hs

#DrIFT-cabalized -o Language/CIL.hs CIL.hs

.PHONY: clean
clean:
	-cd dumpcil && make clean-all
	-rm *.o *.hi
	-rm Language/*.o Language/*.hi
	-rm test
	-rm ciltypes
	-rm cil_types_nocomments.mli
	-rm dump_cil.ml
	-rm CIL.hs
	-rm Language/CIL.hs
	-rm dumpcil/dump_cil.ml

