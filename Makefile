build:
	stack build wfc

run: build
	stack exec wfc

.PHONY: test
test:
	stack build wfc --test --force-dirty --fast

build:
	stack build wfc

runP: build 
	stack exec wfc +RTS -xc

repl: 
	stack ghci src/*
