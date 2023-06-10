build:
	stack build

install:
	cp bin/revolut.sh ~/.local/bin
	stack install

format:
	ormolu --mode inplace $$(git ls-files '*.hs')

test-watch:
	ghcid --command 'stack ghci envelopes --test --main-is envelopes:test:envelopes-test' --test 'main' --warnings

repl:
	TERM=dumb stack ghci
