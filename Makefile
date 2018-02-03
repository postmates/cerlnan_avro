REBAR3 = $(CURDIR)/bin/rebar3
.DEFAULT_GOAL	:= all

.PHONY: all compile clean-all clean clobber test check deps rel shell xref dialyzer eunit shell

all: compile

compile: deps
	@$(REBAR3) compile

deps:
	@$(REBAR3) deps

clean-all: clean

clean:
	@$(REBAR3) clean
	rm -rf apps/*/cover

clobber: clean
	rm -rf _build

check: xref dialyzer unit ;

dialyzer:
	@$(REBAR3) dialyzer

unit:
	@$(REBAR3) eunit --cover

xref:
	@$(REBAR3) xref graph

shell: compile
	@$(REBAR3) shell
