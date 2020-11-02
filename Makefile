.PHONY: all test clean compile run check coveralls
	
all:
	ulimit -n 4096; ERL_FLAGS=" -args_file ${PWD}/config/vm.args.src" rebar3 shell --apps ebanx
	
test:
	rebar3 as test ct --cover && rebar3 as test cover --verbose
	
clean:
	rebar3 clean
	
compile:
	rebar3 compile
	
check:
	rebar3 as test dialyzer

coveralls: test
	rebar3 coveralls send
