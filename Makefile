
all:
	stack build

repl:
	stack repl

prof:
	stack build --profile
	#stack exec -- wireboard-exe +RTS -p -hc
	./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/wireboard-exe/wireboard-exe +RTS -p -hc


lint:
	hlint .

run:
	stack run