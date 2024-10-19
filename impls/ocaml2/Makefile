OCAMLC = ocamlc
OCAMLOPT = ocamlfind ocamlopt -package re -linkpkg

SOURCES = reader.ml printer.ml
OBJECTS = $(SOURCES:%.ml=.cache/%.cmx)
STEPS = step0_repl step1_read_print.ml
STEPS_BINS = $(STEPS:%.ml=%)

EXECUTABLE = my_program

all: $(STEPS_BINS)

$(STEPS_BINS): %: %.cmx $(OBJECTS)
	$(OCAMLOPT) $(OBJECTS) $< -o $@

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

clean:
	rm -rf $(OBJECTS) $(STEPS_BINS) *.cmo *.cmx *.cmi *.o
