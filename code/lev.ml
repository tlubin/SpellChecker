open Score
open Nfa
open Dfa
open Type
open Dict

module type LEV =
sig

  (** given a string and dictionary, return back a list of string matches
      that are in the dictionary and "closest" in terms of edit distance.
      also give back scoring information *)
  val find_matches : string -> Dict.t -> (string*Score.score) list

  val unit_tests: unit -> unit

end

module Levenshtein (Nfa: NFA) (Dfa: DFA) (D: DICT with type t = Dict.t) : LEV =
struct

  (* how far to potentially search for a match *)
  let max_edit = 5;;

  (* given a dfa state and an nfa transition, add the appropriate dfa transition *)
  let add_tran my_dfa (tran: nfa_tran) (orig: dfa_state) (dest: dfa_state) : Dfa.t = 
    match tran with
      | Anyi | Anys -> Dfa.add_transition my_dfa orig Other dest
      | NCorrect c -> Dfa.add_transition my_dfa orig (DCorrect c) dest
      | Epsilon -> failwith "bad call to add_tran"

  (* powerset construction to convert an nfa to a dfa *)
  let to_dfa (my_nfa : Nfa.t) : Dfa.t =
    (* create a singleton dfa from the expanded nfa start state *)
    let my_dfa = Dfa.singleton (Nfa.expand my_nfa 
          (NfaStateSet.singleton (Nfa.start_state my_nfa))) in
    (* a frontier to keep track of dfa states to explore *)
    let frontier = [Dfa.start_state my_dfa] in
    (* a set to keep track of dfa states that have been seen already *)
    let seen = DfaStateSet.empty in
    (* add a given set of nfa transitions from a dfa state *)
    let rec add_transitions my_dfa (origin: dfa_state) (trans : nfa_tran list)
  (frontier: dfa_state list) (seen: DfaStateSet.t) : Dfa.t  =
      match trans with
  | [] -> 
    (* build the rest of the dfa using the same frontier and seen *)
    build_dfa my_dfa frontier seen
  | tran::tl ->
    (match tran with
      | Epsilon -> add_transitions my_dfa origin tl frontier seen
      | NCorrect _ | Anyi | Anys ->
        (* get the set of nfa states reachable from the dfa state (set of nfa states) *)
        let new_state : NfaStateSet.t = Nfa.next_state my_nfa origin tran in
        if not (DfaStateSet.mem new_state seen) then
    (* add it to seen and the frontier *)
    let seen = DfaStateSet.add new_state seen in
    let frontier = new_state::frontier in
    if Nfa.has_final my_nfa new_state then
      (* add a transition to the dfa and set the final state *)
      let my_dfa = Dfa.add_final my_dfa new_state in
      let my_dfa = add_tran my_dfa tran origin new_state in
      add_transitions my_dfa origin tl frontier seen
    else 
      (* just add the transition *)
      let my_dfa = add_tran my_dfa tran origin new_state in
      add_transitions my_dfa origin tl frontier seen
              else
    (* don't add anything to the frontier, add the transition *)
    let my_dfa = add_tran my_dfa tran origin new_state in
    add_transitions my_dfa origin tl frontier seen)
    (* matches on the frontier and calls add_transitions until the frontier is empty *)
    and build_dfa my_dfa (frontier: dfa_state list)  (seen: DfaStateSet.t) : Dfa.t =
      match frontier with
  | [] -> my_dfa
  | current::tl ->
    let transitions : nfa_tran list = Nfa.get_transitions my_nfa current in
    add_transitions my_dfa current transitions tl seen
    in build_dfa my_dfa frontier seen

  (* given an edit distance, find all matches at that edit distance *)
  let find_all_matches word distance dict =
    (* build an nfa from the word and edit distance *)
    let word_nfa = Nfa.build word distance in
    (* convert the nfa to a dfa *)
    let word_dfa = to_dfa(word_nfa) in
    (* zigzag back and forth between the dictionary and the dfa *)
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

  let find_matches word dict =
    let rec get_matches edit_d =
      (* set a cut off of how far to search *)
      if edit_d >= max_edit then []
      else
  (* get all matches at a certain edit distance *)
  let matches = find_all_matches word edit_d dict in
  match matches with
    | [] -> 
      (* search further *)
      get_matches (edit_d+1)
    | ms -> 
      (* keep searching under a set of conditions *)
      if List.length ms <= 3 && edit_d <> 0 && Score.all_low ms word
      then get_matches (edit_d+1) else ms
    in 
    let matches = get_matches 0 in
    Score.get_score matches word

  let unit_tests () = 
    let nfa = Nfa.build "food" 1 in
    let dfa = to_dfa nfa in
    Dfa.print_dfa dfa;

end
