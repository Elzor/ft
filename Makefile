ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR = $(shell which rebar3)

ifeq ($(REBAR),)
$(error "rebar3 not available on this system")
endif

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib
export REBAR_COLOR:="low"


# use to override vars for your platform
ifeq (env.mk,$(wildcard env.mk))
	include env.mk
endif

.PHONY: deps test

clean_old_proc:
	@pkill beam.smp || true
	@pkill epmd || true
	@epmd -daemon

all: $(REBAR) compile

compile: $(REBAR)
	$(REBAR) compile

test: $(REBAR) clean_old_proc
	@$(REBAR) as test eunit
	@$(REBAR) as test ct --sys_config ./config/test.config --spec ./test/specs/all.spec --sname tests --readable true --basic_html false

docs: $(REBAR)
	@$(REBAR) edoc

lint:
	@$(REBAR) as lint lint

xref:
	@$(REBAR) as prod xref skip_deps=true

dialyzer:
	@$(REBAR) dialyzer skip_deps=true

check: lint dialyzer xref test

deps:
	@$(REBAR) deps
	@$(REBAR) vendor store

clean:
	$(REBAR) clean

auto:
	$(REBAR) auto

rel:
	$(REBAR) release -n ft