# Makes our program
PROG = spellchecker

# Setup
LIBS = \

CAMLC = ocamlc
CAMLDOC = ocamldoc
CAMLFLAGS =

%.cmo: %.ml
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

OBJ = $(SRC:.ml=.cmo)

#Executable:

$(PROG): $(OBJ)
	$(CAMLC) $(CAMLFLAGS) unix.cma $(OBJ) -o $(PROG)

doc: $(OBJ)
	$(CAMLDOC) -html $(SRC)

# Other

all: $(PROG)

clean: 
	rm -rf *.cmo *.cmi *.html *.css $(PROG)

.DEFAULT_GOAL := $(PROG)
.PHONY: doc build run clean
