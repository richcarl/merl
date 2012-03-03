# simple Makefile
VSN=0.9
ERLC_FLAGS=-pa ./ebin
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
DOC_OPTS={def,{version,\"$(VSN)\"}}

all: $(OBJECTS) test
ebin/%.beam: src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

# special dependencies due to parse transform
ebin/merl_tests.beam ebin/merl_build.beam: \
	ebin/merl_transform.beam ebin/merl.beam


clean:
	-rm $(OBJECTS)
test:
	erl -noshell -pa ebin \
	 -eval 'eunit:test("ebin",[])' \
	 -s init stop

release: clean
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -DNOTEST"

docs:
	erl -pa ./ebin -noshell -eval "edoc:application(merl, \".\", [$(DOC_OPTS)])" -s init stop
