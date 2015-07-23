GHC=ghc
GHCFLAGS=-Wall

build: Main

deep-clean: clean
	$(RM) Main

clean: shallow-clean
	$(RM) *.o

shallow-clean:
	$(RM) *.hi

run: Main
	./Main

Main: Main.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
