.PHONY: all
all: Language/CIL.hs

gencil: GenCIL.hs
	ghc --make -W -fglasgow-exts -o gencil GenCIL.hs

CIL.hs: gencil
	./gencil

Language/CIL.hs: CIL.hs
	mkdir -p Language && cp CIL.hs Language/CIL.hs

#DrIFT-cabalized -o Language/CIL.hs CIL.hs

test: Test.hs Language/CIL.hs
	ghc --make -W -fglasgow-exts -o test Test.hs

.PHONY: clean
clean:
	-rm *.o *.hi
	-rm Language/*.o Language/*.hi
	-rm test
	-rm gencil
	-rm cil_types_nocomments.mli
	-rm CIL.hs
	-rm Language/CIL.hs
	-rm -rf install-dumpcil-plugin

