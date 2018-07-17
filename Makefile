lazy-random-bytestring:
	stack build

prof: lazy-random-bytestring.prof

lazy-random-bytestring.prof: build-profile
	stack exec -- lazy-random-bytestring 1000000 +RTS -p | wc -c && head $@ | grep 'total alloc'

build-profile:
	stack build --profile  

.PHONY: lazy-random-bytestring prof build-profile