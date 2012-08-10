REBAR := ./rebar

.PHONY: all deps doc test clean distclean release start testclient

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
	exec erl -args_file rel/files/vm.args -config rel/files/sys.config -pa $(PWD)/deps/*/ebin -pa $(PWD)/ebin -s lager -s bedrock

release: all
	dialyzer --src src/*.erl deps/*/src/*.erl

testclient: 
	exec node test-client/app.js