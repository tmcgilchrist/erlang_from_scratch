ERL		?= erl
ERLC		= erlc
EBIN_DIRS	:= $(wildcard deps/*/ebin)
APPS		:= $(shell ls src)
REL_DIR     	= rel
NODE		= {{name}}
REL		= {{name}}
SCRIPT_PATH  	:= $(REL_DIR)/$(NODE)/bin/$(REL)

.PHONY: rel deps

all: deps compile

compile: deps
	@rebar compile

shell:
	@erl -pa ebin include deps/*/ebin deps/*/include ebin include -boot start_sasl -s reloader -s erlio

deps:
	@rebar get-deps
	@rebar check-deps

clean:
	@rebar clean
