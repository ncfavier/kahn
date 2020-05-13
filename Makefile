.PHONY: Integers
Integers:
	ghc -O -outputdir build -main-is Integers --make $(GHCFLAGS) -o $@ Integers.hs

.PHONY: clean
clean:
	rm -rf build Integers
