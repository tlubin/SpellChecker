open lev

module MyLev = Levenshtein (Nfa.Nfa) (Dfa.Dfa) (Dict.Dict);;

let dict = Sys.argv.(1);
MyLev.find_matches "nice" dict
