lazy-random-bytestring:
	stack build

prof: build-profile
	stack exec -- lazy-random-bytestring 1000000 +RTS -p | wc -c

build-profile:
	stack build --profile  
