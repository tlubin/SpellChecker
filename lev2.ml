module type LEV =
sig
  val find_matches : string -> int -> string -> string list
end

module Levenshtein (Nfa: AUTOMATA) (Dfa: AUTOMATA) (Dict: DICT) : LEV =
struct
  let build_nfa str edit_d : Nfa.nfa_t =
    (* build an Nfa from a string and edit distance *)
    let my_nfa = ref (Nfa.singleton()) in
    let len = String.length str in
    let add_edges () =
      let i = ref 0 in
      let e = ref 0 in
      while !i < len do
	while !e <= edit_d do
          List.iter (fun t -> 
	    let current = (State.state_of_indices !i !e) in
	    match State.transition current t str edit_d with
	      | None -> ()
	      | Some (st,tr) -> transitions := add_transition !transitions current tr st)
	    (State.list_of_trans());
	  e := !e + 1
	done;
	i := !i + 1;
	e := 0
      done in
    let add_final_states () =
      let e = ref 0 in
      while !e <= edit_d do
	final_states := (StateSet.add (State.state_of_indices len !e) (!final_states));
	if !e < edit_d then
	  let current = (State.state_of_indices len !e) in
	  let st, tr = State.transition_final current in
	  transitions := (add_transition !transitions current tr st);
	  e := !e + 1
	else 
	  e := !e + 1
      done in
    add_edges();
    add_final_states();



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
