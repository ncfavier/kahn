.PHONY: Kahn
Kahn:
	ghc -O -outputdir build --make $(GHCFLAGS) -o $@ Main.hs

.PHONY: clean
clean:
	rm -rf build Kahn

.PHONY: archive
archive:
	tar -cvzf favier.tgz --transform 's,^,favier/,' *.hs Makefile LISEZMOI.pdf

%.pdf: %.md
	pandoc -V fontsize=12pt --highlight-style monochrome $< -o $@