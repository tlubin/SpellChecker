(* this is going to be the main file for the command line thing *)

module MyLev = Lev.Levenshtein (Nfa.Nfa) (Dfa.Dfa) (Dict.Dict)

MyLev.find_matches "nice" 1 "usr/bin/lkjasdflkas"
