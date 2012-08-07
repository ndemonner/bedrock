REBAR := ./rebar

.PHONY: all deps doc test clean release

all: deps
	$(REBAR) compile

app:
	$(REBAR) compile skip_deps=true

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps

start: app
	exec erl -args_file rel/files/vm.args -config rel/files/sys.config -pa $(PWD)/deps/*/ebin -pa $(PWD)/apps/*/ebin

release: all
	dialyzer --src src/*.erl deps/*/src/*.erl