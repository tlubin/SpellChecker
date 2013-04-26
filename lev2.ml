module type LEV =
sig
  val find_matches : string -> int -> string -> string list
end

module Levenshtein (Nfa: AUTOMATA) (Dfa: AUTOMATA) (Dict: DICT) : LEV =
struct
  let build_nfa str k : Nfa.nfa_t =
    (* build an Nfa from a string and edit distance *)
    failwith "sjdlflksa"

  let next_valid_string (my_dfa : Dfa.dfa_t) str : string =
    failwith "askldjflska"

  let expand my_nfa states : (Nfa.state set) =
    failwith "aslkfjasldk"

  let to_dfa (my_nfa : Nfa.nfa_t) : Dfa.dfa_t =
    failwith "sadfasdf"

  let find_matches word distance dict_file =
    let word_nfa = build_nfa word distance in
    let word_dfa = to_dfa(word_nfa) in
    let mydict = Dict.create dict_file in
    let rec find_matches_rec (current: string) (matches : string list) =
      match next_valid_string word_dfa current with
	| Some str ->
	  let next_dict = Dict.next_entry mydict str in
	  if next_dict = "" then matches
	  else 
	    if next_dict = str 
	    then find_matches_rec (str ^ (Char.escaped(Dict.first_letter()))) (str::matches)
	    else find_matches_rec next_dict matches
	| None -> matches
    in find_matches_rec "" []

end

module 
module Nfa = Automata (MyNfaState)
module Dfa = Automata (MyDfaState)
module MyLev = Levenshtein (Nfa) (Dfa) (Dict.Dict)
