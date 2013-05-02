open Nfa
open Dfa
open Type
open Dict

module type LEV =
sig
  type dict_t
  val create_dict : string -> dict_t
  val find_matches : string -> int -> dict_t -> string list
  val find_matches_time : string -> int -> dict_t -> string list
end

module Levenshtein (Nfa: NFA) (Dfa: DFA) (D: DICT) : LEV =
struct

  type dict_t = D.t
  let create_dict = D.create

  let add_tran my_dfa (tran: nfa_tran) (orig: dfa_state) (dest: dfa_state) = 
    match tran with
      | Anyi | Anys -> Dfa.add_transition my_dfa orig Other dest
      | NCorrect c -> Dfa.add_transition my_dfa orig (DCorrect c) dest
      | Epsilon -> failwith "bad call to add_tran"

  let to_dfa (my_nfa : Nfa.t) : Dfa.t =
    let my_dfa = Dfa.singleton (Nfa.expand my_nfa 
				  (NfaStateSet.singleton (Nfa.start_state my_nfa))) in
    let frontier = [Dfa.start_state my_dfa] in
    let seen = DfaStateSet.empty in
    let rec add_transitions my_dfa (origin: dfa_state) (trans : nfa_tran list)
	(frontier: dfa_state list) (seen: DfaStateSet.t) : Dfa.t  =
      match trans with
	| [] -> build_dfa my_dfa frontier seen
	| tran::tl ->
	  (match tran with
	    | Epsilon -> add_transitions my_dfa origin tl frontier seen
	    | NCorrect _ | Anyi | Anys ->
	      let new_state : NfaStateSet.t = Nfa.next_state my_nfa origin tran in
	      if not (DfaStateSet.mem new_state seen) then
		let seen = DfaStateSet.add new_state seen in
		let frontier = new_state::frontier in
		if Nfa.has_final my_nfa new_state then
		  let my_dfa = Dfa.add_final my_dfa new_state in
		  let my_dfa = add_tran my_dfa tran origin new_state in
		  add_transitions my_dfa origin tl frontier seen
		else 
		  let my_dfa = add_tran my_dfa tran origin new_state in
		  add_transitions my_dfa origin tl frontier seen
              else
		let my_dfa = add_tran my_dfa tran origin new_state in
		add_transitions my_dfa origin tl frontier seen)
    and build_dfa my_dfa (frontier: dfa_state list)  (seen: DfaStateSet.t) : Dfa.t =
      match frontier with
	| [] -> my_dfa
	| current::tl ->
	  let transitions : nfa_tran list = Nfa.get_transitions my_nfa current in
	  add_transitions my_dfa current transitions tl seen
    in build_dfa my_dfa frontier seen


  let find_matches word distance dict =
    let word_nfa = Nfa.build word distance in
    let word_dfa = to_dfa(word_nfa) in
    let rec find_matches_rec (current: string) (matches : string list) =
      match Dfa.next_valid_string word_dfa current with
	| Some str ->
	  let next_dict = D.next_entry dict str in
	  if next_dict = "" then matches
	  else 
	    if next_dict = str 
	    then find_matches_rec (str ^ (Char.escaped(D.first_letter))) (str::matches)
	    else find_matches_rec next_dict matches
	| None -> matches
    in find_matches_rec "" []

  let find_matches_time word distance dict =
    let word_nfa = Nfa.build word distance in
    let t2 = Unix.gettimeofday() in
    let word_dfa = to_dfa(word_nfa) in
    let t3 = Unix.gettimeofday() in
    Printf.printf "time to convert nfa to dfa: %f\n" (t3-.t2);
    let t4 = Unix.gettimeofday() in
    Printf.printf "time to build the dictionary: %f\n" (t4-.t3);
    let rec find_matches_rec (current: string) (matches : string list) =
      match Dfa.next_valid_string word_dfa current with
	| Some str ->
	  let next_dict = D.next_entry dict str in
	  if next_dict = "" then matches
	  else 
	    if next_dict = str 
	    then find_matches_rec (str ^ (Char.escaped(D.first_letter))) (str::matches)
	    else find_matches_rec next_dict matches
	| None -> matches in
    let answer = find_matches_rec "" [] in
    let t5 = Unix.time() in
    Printf.printf "time to find matches: %f\n" (t5-.t4);
    answer

end
