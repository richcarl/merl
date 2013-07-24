ERLC_FLAGS=+debug_info
SOURCES=lisp.erl lispc.erl basic.erl basicc.erl
INCLUDES=../include
HEADERS=$(INCLUDES)/merl.hrl
OBJECTS=$(SOURCES:%.erl=%.beam)

all: $(OBJECTS)

%.beam: %.erl $(HEADERS) Makefile
	erlc -pa ../ebin $(ERLC_FLAGS) -o ./ $<

# additional dependencies due to the parse transform
lispc.beam basicc.beam: ../ebin/merl_transform.beam ../ebin/merl.beam

clean:
	-rm -f $(OBJECTS)
