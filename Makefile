# simple Makefile
ERLC_FLAGS=-pa ./ebin
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
all: $(OBJECTS) test
ebin/%.beam: src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

ebin/merl_tests.beam: ebin/merl_transform.beam

clean:
	-rm $(OBJECTS)
test:
	erl -noshell -pa ebin \
	 -eval 'eunit:test("ebin",[])' \
	 -s init stop

release: clean
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -DNOTEST"
