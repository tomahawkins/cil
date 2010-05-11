.PHONY: all
all: Language/CIL.hs

Language/CIL.hs: Language/CIL.DrIFT.hs
	DrIFT-cabalized -o Language/CIL.hs Language/CIL.DrIFT.hs

test: Test.hs Language/CIL.hs
	ghc --make -W -fglasgow-exts -o test Test.hs

ciltypes: CILTypes.hs
	ghc --make -W -fglasgow-exts -o ciltypes CILTypes.hs

.PHONY: clean
clean:
	-rm *.o *.hi
	-rm Language/*.o Language/*.hi
	-rm test
	-rm ciltypes

.PHONY: clean-all
clean-all: clean
	-rm Language/CIL.hs
