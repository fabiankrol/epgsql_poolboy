TEST_DATABASE = epgsql_test_database
.PHONY: deps test doc

all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

distclean: clean
	rebar delete-deps

databases: $(TEST_DATABASE)
$(TEST_DATABASE):
	@if [ `psql -l | grep $@ | wc -l` -gt 0 ]; then \
		createdb $@; \
	fi

postgres-init: databases
	@psql -d $(TEST_DATABASE) < priv/test_schema.sql

test: postgres-init
	rebar skip_deps=true ct

dialyzer: compile
	@dialyzer -Wno_undefined_callbacks \
        -r ebin \
		-r deps/bear \
		-r deps/epgsql \
        -r deps/folsom \
        -r deps/poolboy
