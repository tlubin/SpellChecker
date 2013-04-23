open Type

module type NFA =
sig
  type nfa_t

  (** build an nfa_t for a given input word and edit distance *)
  val build: string -> int -> nfa_t
    (* stuff for powerset construction *)
    (* add_state, ... *)

  (* takes nfa and returns start state *)
  val start_state: nfa_t -> state

  (* gets next state of nfa *)
  val next_state: nfa_t -> state -> nfa_tran -> state

  (* get all transitions (list) leaving one state *)
  val get_transitions: nfa_t -> state -> nfa_tran list

  (* bool, check if a state is a final state *)
  val is_final: nfa_t -> state -> bool 


end

module Nfa : NFA =
struct
  (* transitions dictionary * starting state * final states *)
  type nfa_t = n_automata

  (* return the start state of an nfa *)
  let start_state my_nfa = 
    let _, start, _ = my_nfa in start

  (* get the next state given an origin and transition type *)
  let next_state my_nfa orig trans =
    let trans_dict, start, final_states = my_nfa in
    assert(StateDict.mem orig trans_dict);
    let inner_dict = StateDict.find orig trans_dict in
    assert(NTranDict.mem trans inner_dict);
    NTranDict.find trans inner_dict

  (* return a list of transitions from a given state *)
  let get_transitions my_nfa orig =
    let trans_dict, _, _ = my_nfa in 
    assert(NTranDict.mem orig trans_dict);
    NTranDict.fold (fun tran _ trans -> tran::trans) trans_dict []

  (* return whether a given state is a final state *)
  let is_final my_nfa state =
    let _, _, final_states = my_nfa in
    StateSet.mem state final_states


  (* array of transition types to use later 
   * '$' - arbitrary char
   *)
  let tran_types = [ Delete ; Insert ; Swap ; Actual '$']

  (* Build NFA Helpers *)
  let add_transition (transitions: n_inner StateDict.t) (src: state) 
    (tran: nfa_tran) (dest: state) =
    (* check if our starting state already exists in state dictionary *)
    (* do asserts here later for overwriting *)
    if StateDict.mem src transitions then
      let inner_dict = StateDict.find src transitions in
      let inner_dict = NTranDict.add tran dest inner_dict in
      StateDict.add src inner_dict transitions
    else
      let t_dict = NTranDict.singleton tran dest in
      StateDict.add src t_dict transitions

  (* Build NFA Main Function *)
  let build (str: string) (edit_d: int) =
    (* initialize starting state, final_states set, transitions dictionary *)
    let starting_state = (0,0) in
    let final_states = ref StateSet.empty in
    let transitions = ref (StateDict.singleton starting_state NTranDict.empty) in
    
    (* Now iteratively build_from_string *)
    let build_from_string str: unit =
      let i = ref 0 in
      let e = ref 0 in
      let len = String.length str in
      while !i < len do
        while !e <= edit_d do
          List.iter (fun t_type -> 
            match t_type with
            | Actual _ -> 
                transitions := (add_transition !transitions (len, !e) (Actual (String.get str !i)) (len, !e+1))
            | Delete -> 
                transitions := (add_transition !transitions (!i,!e) Delete (!i, !e+1))
            | Insert -> 
                transitions := (add_transition !transitions (!i,!e) Insert (!i+1, !e+1)) 
            | Swap ->
                transitions := (add_transition !transitions (!i,!e) Swap (!i+1, !e+1))
          ) tran_types;
          e := !e + 1
        done;
        final_states := StateSet.add (len, !e) (!final_states);
        i := !i + 1;
        e := 0
      done
    in
      (build_from_string str);
      (!transitions, starting_state, !final_states)
end
  
