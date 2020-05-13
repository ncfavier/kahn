.PHONY: Kahn
Kahn:
	ghc -O -outputdir build --make $(GHCFLAGS) -o $@ Main.hs

.PHONY: clean
clean:
	rm -rf build Kahn
