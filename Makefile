# Makes our program
PROG = spellchecker

# Setup
LIBS = \

CAMLOPT = ocamlopt
CAMLDOC = ocamldoc
STR = str.cmxa

%.cmx: %.ml
	$(CAMLOPT) -c $<


# Source and Object files
SRC = \
	dict.ml \
	type.ml \
	ranker.ml \
	qwerty.ml \
	score.ml \
	automata.ml \
	dfa.ml \
	nfa.ml \
	lev.ml \
	menu.ml \
	main.ml \


OBJ = $(SRC:.ml=.cmx)

#Executable:

$(PROG): $(OBJ)
	$(CAMLOPT) $(STR) $(OBJ) -o $(PROG)

doc: $(OBJ)
	$(CAMLDOC) -html $(SRC)

# Other

all: $(PROG)

clean: 
	rm -rf *.cmo *.cmi *.cmx *.o *.html *.css $(PROG) $(PROGOPT)