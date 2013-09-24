.PHONY: all deps update-deps get-deps compile test clean build_plt check_plt dialyzer

REBAR_CONFIG = rebar.config

all: clean deps test

deps: get-deps update-deps
	@./rebar -C $(REBAR_CONFIG) compile

update-deps:
	@./rebar -C $(REBAR_CONFIG) update-deps

get-deps:
	@./rebar -C $(REBAR_CONFIG) get-deps

compile:
	@./rebar -C $(REBAR_CONFIG) compile skip_deps=true
	@./rebar -C $(REBAR_CONFIG) xref skip_deps=true

test:
	@./rebar -C $(REBAR_CONFIG) eunit skip_deps=true

clean:
	@./rebar -C $(REBAR_CONFIG) clean skip_deps=true

APPS = erts kernel stdlib sasl crypto
PLT = $(HOME)/.tsbox_dialyzer_plt

build_plt:
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS)

check_plt: compile
	dialyzer --check_plt --plt $(PLT) --apps $(APPS)

dialyzer: compile
	dialyzer -Wno_return -Wunmatched_returns \
	         -Werror_handling \
	         -Wrace_conditions \
	         -Wunderspecs\
		--plt $(PLT) deps/*/ebin ebin | \
	    tee .dialyzer.raw-output | egrep -v -f ./dialyzer.ignore-warnings
