lazy-random-bytestring:
	stack build

prof: lazy-random-bytestring.prof

lazy-random-bytestring.prof: build-profile
	stack exec -- lazy-random-bytestring 1000000 +RTS -p | wc -c && head $@ | grep 'total alloc'

build-profile:
	stack build --profile  

bin/lazy-random-bytestring: Main.hs
	stack install --local-bin-path bin
	
valgrind: bin/lazy-random-bytestring
	valgrind --tool=massif $<

.PHONY: lazy-random-bytestring prof build-profile valgrind
