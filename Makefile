
MIX = MIX
.DEFAULT_GOAL	:= all

.PHONY: all compile clean-all clean clobber test check deps rel shell xref dialyzer eunit exunit erl iex

all: compile

compile: deps
	@$(MIX) compile

deps:
	@$(MIX) deps.get

clean-all: clean

clean:
	@$(MIX) clean
	rm -rf apps/*/cover

clobber: clean
	rm -rf _build

check: xref dialyzer eunit exunit ;

dialyzer:
	@$(MIX) dialyzer

exunit:
	@$(MIX) test --cover

eunit:
	@$(MIX) eunit --cover

proper:
	@$(MIX) proper

xref:
	@$(MIX) xref graph

iex: compile
	iex -S mix

erl: compile
	erl -pa ./_build/dev/lib/*/ebin
