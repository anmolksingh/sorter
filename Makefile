ERL ?= erl
APP := sorter
GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)

.PHONY: deps

all: deps
	@./rebar compile

rel: all
	@./rebar generate

app:
	@./rebar compile skip_deps=true
	@python reload.py

deps:
	@./rebar get-deps

clean:
	@./rebar clean

relclean: clean
	@rm -rf rel/$(APP)
	find . -type f -name "*.beam" -exec rm -f {} \;

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
