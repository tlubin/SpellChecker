# Makes our program
PROG = spellchecker

# Setup
LIBS = \

CAMLC = ocamlopt
CAMLDOC = ocamldoc
CAMLFLAGS =

%.cmx: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

# Source and Object files
SRC = \
	dict.ml \
	type.ml \
	automata.ml \
	dfa.ml \
	nfa.ml \
	lev.ml \
	main.ml \
	qwerty.ml \

OBJ = $(SRC:.ml=.cmx)

#Executable:

$(PROG): $(OBJ)
	$(CAMLC) $(CAMLFLAGS) $(OBJ) -o $(PROG)

doc: $(OBJ)
	$(CAMLDOC) -html $(SRC)

# Other

all: $(PROG)

clean: 
	rm -rf *.cmo *.cmi *.cmx *.o *.html *.css $(PROG)

.DEFAULT_GOAL := $(PROG)
.PHONY: doc build run clean
