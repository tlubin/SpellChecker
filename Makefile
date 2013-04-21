# Makes our program
PROG = spellchecker

# Setup
LIBS = \

CAMLC = ocamlc
CAMLDOC = ocamldoc
CAMLFLAGS = -g

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

# Source and Object files
SRC = \
	main.ml \
	type.ml \
	lev.ml \
	dict.ml \
	dfa.ml \
	nfa.ml \

OBJ = $(SRC:.ml=.cmo)

#Executable:

$(PROG): $(OBJ)
	$(CAMLC) $(CAMLFLAGS) $(OBJ) -o $(PROG)

doc: $(OBJ)
	$(CAMLDOC) -html $(SRC)

# Other

all: $(PROG)

clean: 
	rm -rf *.cmo *.cmi *.html *.css $(PROG)

.DEFAULT_GOAL := $(PROG)
.PHONY: doc build run clean
