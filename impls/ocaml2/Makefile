OCAMLC = ocamlc
OCAMLOPT = ocamlfind ocamlopt -package re -linkpkg

SOURCES = reader.ml printer.ml env.ml
OBJECTS = $(SOURCES:%.ml=%.cmx)
STEPS = step0_repl step1_read_print.ml step2_eval.ml step3_env.ml
STEPS_BINS = $(STEPS:%.ml=%)

EXECUTABLE = my_program

all: $(STEPS_BINS)

$(STEPS_BINS): %: $(OBJECTS) %.cmx
	$(OCAMLOPT) $(OBJECTS) $@.cmx -o $@

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

clean:
	rm -rf $(OBJECTS) $(STEPS_BINS) *.cmo *.cmx *.cmi *.o
