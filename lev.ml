module type LEV =
sig
  val find_matches : string -> int -> string -> string list
end

module Levenshtein (nfa: NFA) (dfa: DFA) (dict: DICT) : LEV =
struct

  let find_matches (word: string) (distance: int) (dict_file: string) =
    let mydict : dict.dict_t = dict.create dict_file in
    let word_nfa : nfa.nfa_t = nfa.build(word) in
    let word_dfa = to_dfa(word_nfa) in
    let rec find_matches_rec (current: string) (matches : string list) =
      let next_match = word_dfa.next_valid_string current in
      let next_dict = mydict.next_entry next_match in
      if next_dict = next_match then find_matches_rec next_match (next_match::matches)
      else failwith "implement me"
    in find_matches_rec '' []
	
  let to_dfa (nfa: nfa.nfa_t) : dfa.dfa_t =
    (* convert nfa to dfa *)
    let dfa = dfa.create in
    failwith "implement me"
    (* build this with whatever functions in dfa and nfa module *)

end
