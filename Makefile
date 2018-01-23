
MIX = MIX
.DEFAULT_GOAL	:= all

.PHONY: all compile clean-all clean clobber test check deps rel shell xref dialyzer eunit erl iex

all: compile

compile: deps
	@$(MIX) compile

deps:
	@$(MIX) deps.get

clean-all: clean

clean:
	@$(MIX) clean

clobber: clean
	rm -rf _build

check: xref dialyzer eunit ;

dialyzer:
	@$(MIX) dialyzer

eunit:
	@$(MIX) test --cover

proper:
	@$(MIX) proper

xref:
	@$(MIX) xref graph

iex: compile
	iex -S mix

erl: compile
	erl -pa ./_build/dev/lib/*/ebin
