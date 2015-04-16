all:
	cabal run

tags:
	@ cabal clean
	@ echo ":ctags" | cabal repl -v0

.PHONY: all tags

