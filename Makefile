# Makes our program
PROG = spellchecker
PROGOPT = spellcheckeropt

# Setup
LIBS = \

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDOC = ocamldoc
CAMLFLAGS = -g

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

%.cmx: %.ml
	$(CAMLOPT) -c $<


# Source and Object files
SRC = \
	dict.ml \
	type.ml \
	ranker.ml \
	automata.ml \
	dfa.ml \
	nfa.ml \
	lev.ml \
	main.ml \

OBJO = $(SRC:.ml=.cmo)
OBJX = $(SRC:.ml=.cmx)

#Executable:

$(PROG): $(OBJO)
	$(CAMLC) $(CAMLFLAGS) $(OBJO) -o $(PROG)

$(PROGOPT): $(OBJX)
	$(CAMLOPT) $(OBJX) -o $(PROGOPT)

doc: $(OBJO)
	$(CAMLDOC) -html $(SRC)

# Other

all: $(PROG)

opt: $(PROGOPT)

clean: 
	rm -rf *.cmo *.cmi *.cmx *.o *.html *.css $(PROG) $(PROGOPT)

#.DEFAULT_GOAL := $(PROG)
#.PHONY: doc build run clean opt