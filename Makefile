TEST_DATABASE = epgsql_test_database
REBAR = ./rebar


.PHONY: deps test doc

all: deps compile

compile:
	$(REBAR) compile

compile-fast:
	$(REBAR) compile skip_deps=true

console:
	erl -pa deps/*/ebin/ -pa ebin/ -sname epgsql_poolboy

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps

databases: $(TEST_DATABASE)
$(TEST_DATABASE):
	@if [ `psql -l | grep $@ | wc -l` -eq 0 ]; then \
		createdb $@; \
	fi

postgres-init: databases
	@psql -d $(TEST_DATABASE) < priv/test_schema.sql

test: postgres-init
	$(REBAR) skip_deps=true ct

dialyzer: compile
	@dialyzer -Wno_undefined_callbacks \
        -r ebin \
		-r deps/bear \
		-r deps/epgsql \
        -r deps/folsom \
        -r deps/poolboy
