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
	
  let to_dfa (my_nfa: nfa.nfa_t) : dfa.dfa_t =
    let my_dfa = dfa.singleton (nfa.start_state my_nfa) in
    let frontier = [nfa.start_state my_nfa] in
    let seen = Type.StateSet.empty in

    let add_tran my_dfa (tran : Nfa.tran) (orig: Type.state) (dest: Type.state) =
      match tran with
	| Nfa.Insert | Nfa.Swap -> dfa.add_transition my_dfa orig Dfa.Other dest
	| Nfa.Correct c -> dfa.add_transition my_dfa orig (Dfa.Correct c) dest
	| Nfa.Delete -> failwith "shouldn't happen" in

    let rec add_transitions my_dfa (origin: Type.state) (trans : Nfa.tran list)
	(frontier: Type.state list)  (seen: Type.StateSet)  =
      match trans with
	| [] -> my_dfa
	| tran::tl ->
	  (match tran with
	    | Nfa.Delete -> add_transitions my_dfa origin tl frontier seen
	    | Nfa.Correct c | Nfa.Insert | Nfa.Swap ->
	      let new_state : Type.state = nfa.next_state my_nfa origin tran in
	      if  not (Type.StateSet.mem new_state seen) then
		let seen = Type.StateSet.add new_state seen in
		let frontier = new_state::frontier in
		if nfa.is_final my_nfa new_state then
		  let my_dfa = dfa.add_final my_dfa new_state in
		  let my_dfa = add_tran tran origin new_state in
		  add_transitions my_dfa origin tl frontier seen
		else 
		  add_transitions my_dfa origin tl frontier seen in

    let rec build_dfa my_dfa (frontier: Type.state list)  (seen: Type.StateSet) =
      match frontier with
	| [] -> my_dfa
	| current::tl ->
	  let transitions : Nfa.tran list = nfa.get_transitions my_nfa current in
	  add_transitions my_dfa current transitions frontier seen
    in build_dfa my_dfa frontier seen
end
