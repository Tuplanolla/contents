GHC=ghc
GHCFLAGS=-Wall

build: contents contents-config

deep-clean: clean
	$(RM) contents

clean: shallow-clean
	$(RM) *.o

shallow-clean:
	$(RM) *.hi

run: contents contents-config
	./contents
	./contents-config

contents: Contents.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

contents-config: ContentsConfig.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
