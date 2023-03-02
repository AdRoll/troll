REBAR3 := rebar3

.PHONY: all compile check

check:
	$(REBAR3) do compile, dialyzer, lint, xref, hank

compile:
	$(REBAR3) compile

dialyzer:
	$(REBAR3) dialyzer

lint:
	$(REBAR3) lint

xref:
	$(REBAR3) xref

hank:
	$(REBAR3) hank

format:
	$(REBAR3) format
