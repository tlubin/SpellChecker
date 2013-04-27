open Automata
open Type

module type NFA =
sig
  type t

  (* takes in a set of sets and returns a set of states
     reachable within epsilon of the current state *)
  val expand : t -> NfaStateSet.t -> NfaStateSet.t

  val start_state : t -> nfa_state

  (* return a list of transitions coming out of any of the
     states in an input set of states *)
  val get_transitions : t -> NfaStateSet.t -> nfa_tran list

  (* takes in a set of states and returns a new set of states
     reachable by a given transition. i.e. if a Correct is
     given then an Any can be taken as well. Expand the result *)
  val next_state : t -> NfaStateSet.t -> nfa_tran -> NfaStateSet.t

  (* return whether or not a set of states contains a final state *)
  val has_final : t -> NfaStateSet.t -> bool

  val build : string -> int -> t
  val print_nfa : t -> unit

  val unit_tests : unit -> unit
end

module Nfa : NFA =
struct

  module A = Automata (MyNfaState)

  type t = A.t

  let start_state = A.start_state

  let get_transitions my_nfa states = 
    let rec helper states total_trans = 
      if NfaStateSet.cardinal states = 0 then total_trans
      else
	let state = NfaStateSet.choose states in
	let states' = NfaStateSet.remove state states in
	let trans = A.get_transitions my_nfa state in
	(* add all elements from trans into total_trans and avoid duplicates *)
	(* ALGORITHMICALLY BAD!! *)
	let new_total =	List.fold_left (fun total curr -> 
	  if not (List.mem curr total) then curr::total
	  else total) total_trans trans in
	helper states' new_total
    in helper states []
	    
	  

  let has_final my_nfa states = NfaStateSet.exists (A.is_final my_nfa) states

  (* extract the state from an option that is known to be Some s *)
  let extract_state (state: nfa_state option) : nfa_state =
    match state with
      | Some s -> s
      | None -> failwith "bad call to extract_state"

  let expand my_nfa states =
    let rec expand_rec frontier new_states =
      if NfaStateSet.cardinal frontier = 0 then new_states
      else 
	let state = NfaStateSet.choose frontier in
	let frontier = NfaStateSet.remove state frontier in
	match A.next_state my_nfa state Epsilon with
	  | None -> expand_rec frontier new_states
	  | Some s ->
	    if not (NfaStateSet.mem s new_states) then 
	      let frontier = NfaStateSet.add s frontier in
	      expand_rec frontier (NfaStateSet.add s new_states)
	    else
	      expand_rec frontier new_states
    in expand_rec states states

  (* take in a transition and a starting state and return a set of states
     reachable by Any or the passed in transition *)
  let get_dests my_nfa orig t =
    List.fold_left (fun dests tr ->
      match tr with
	| NCorrect c ->
	  if NCorrect c = t then
	    let dest = extract_state (A.next_state my_nfa orig tr) in
	    NfaStateSet.add dest dests
	  else dests
	| Anyi | Anys ->
	  let dest = extract_state (A.next_state my_nfa orig tr) in
	  NfaStateSet.add dest dests
	| Epsilon -> dests) (NfaStateSet.empty) (A.get_transitions my_nfa orig)

  (* takes in a set of states and returns a new set of states
     reachable by a given transition. i.e. if a Correct is
     given then an Any can be taken as well. Expand the result *)
  let next_state my_nfa origs t =
    let rec helper origs dests =
      if NfaStateSet.cardinal origs = 0 then dests
      else 
	(* take a state from origs set *)
	let state = NfaStateSet.choose origs in
	let origs = NfaStateSet.remove state origs in
	(* get destination states *)
	let new_dests = get_dests my_nfa state t in
	(* add destination states to dests set *)
	helper origs (NfaStateSet.union dests new_dests)
    in expand my_nfa (helper origs (NfaStateSet.empty))

  let build str edit_d = 
    let my_nfa = ref (A.singleton_default()) in
    let len = String.length str in
    let add_edges () =
      let i = ref 0 in
      let e = ref 0 in
      while !i < len do
	while !e <= edit_d do
	  (* add transitions from each state *)
          List.iter (fun t_type -> 
            match t_type with
              | NCorrect _ -> 
		my_nfa := (A.add_transition !my_nfa (!i, !e) (NCorrect (String.get str !i)))
		  (!i+1, !e)
              | Epsilon -> if !e < edit_d then 
                  my_nfa := (A.add_transition !my_nfa (!i,!e) Epsilon (!i+1, !e+1))
              | Anyi -> if !e < edit_d then 
                  my_nfa := (A.add_transition !my_nfa  (!i,!e) Anyi (!i, !e+1))
              | Anys -> if !e < edit_d then 
                  my_nfa := (A.add_transition !my_nfa (!i,!e) Anys (!i+1, !e+1))) (A.tran_types());
          e := !e + 1
	done;
	i := !i + 1;
	e := 0
      done in
    let add_final_states () =
      let e = ref 0 in
      while !e <= edit_d do
	my_nfa := A.add_final !my_nfa (len, !e);
	if !e < edit_d then
	  my_nfa := A.add_transition !my_nfa (len, !e) Anyi (len, !e+1);
	e := !e + 1
      done in
    add_edges();
    add_final_states();
    !my_nfa

  let print_nfa = A.print_automata

  let unit_tests () =
    let my_nfa = build "food" 1 in
    let state_set = NfaStateSet.singleton (0,1) in
    let state_set = NfaStateSet.add (1,1) state_set in
    let states = next_state my_nfa state_set (NCorrect 'f') in
    NfaStateSet.iter A.print_state states
    
end
