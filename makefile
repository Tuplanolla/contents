GHC=ghc
GHCFLAGS=-Wall

build: contents

deep-clean: clean
	$(RM) Main

clean: shallow-clean
	$(RM) *.o

shallow-clean:
	$(RM) *.hi

run: contents
	./contents

contents: Main.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
