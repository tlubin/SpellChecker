type tran = Correct of char | Other

module type DFA =
sig
  type dfa_t
  
  (* return the next valid string satisfying the dfa *)
  val next_valid_string: dfa_t -> string -> string

  (* singleton returns a dfa, with given start state *)
  val singleton: Type.state -> dfa_t

  (* add a transition (cur state, transition, new state) to the dfa *)
  val add_transition: 
    dfa_t -> Type.state -> tran -> Type.state -> dfa_t

  (* add to final states *)
  val add_final: dfa_t -> Type.state -> dfa_t 
end

module Dfa : DFA =
struct

  type dfa_t = Type.automata

  module TranDict = Map.Make(
    struct
      type t = Dfa.tran
      let compare a b = compare a b
    end)

  (* Helper Functions for next_valid_string *)
  let start_state my_dfa =
    let _, start, _ = my_dfa in start

  let next_state my_dfa state tran: Type.state option =
    let trans_dict, _, _ = my_dfa in
    assert(Type.StateDict.mem state trans_dict);
    let inner_dict = Type.StateDict.find state trans_dict in
    if not (TranDict.mem tran inner_dict) then None
    else Some TranDict.find tran inner_dict

  let is_final my_dfa state =
    let _, _. final_states = my_dfa in Type.StateSet.mem state final_states

  (* get the next valid lexigraphical string in the DFA *)
  (* INCOMPLETE IMPLEMENTATION *)
  let next_valid_string my_dfa str =
    let state = start_state my_dfa in
    let stack = [] in
    (* evaluate the dfa as far as possible *)
    let rec evaluate_dfa current_state depth stack  =
      if depth = String.length str then (stack*current_state)
      else 
	let letter = String.sub str depth (depth+1) in
	let str_so_far = String.sub str 0 (depth+1) in
	match next_state my_dfa current_state (Correct letter) with
	  | None ->
	    (* add a dummy state of (-1,-1) to the stack *)
	    let state = (-1,-1) in
	    let stack = (str_so_far, state, letter)::stack in
	    (stack, state)
	  | Some s -> 
	    let stack = (str_so_far, s, letter) in
	    evaluate_dfa s (depth+1) stack in
    let stack, state = evaluate_dfa state 0 stack in
    if is_final my_dfa state then str (* word is valid *) 
    else
      failwith "implement the rest of this function"


  (* return a singleton dfa with the given state *)
  let singleton (state: Type.state) = 
    Type.StateDict.singleton state (TranDict.empty)

  (* add a transition from orig to dest using transition type trans *)
  let add_transition my_dfa orig trans dest =
    let trans_dict, start, final_states = my_dfa in
    if Type.StateSet.mem orig trans_dict then 
      (let inner_dict = Type.StateSet.find orig trans_dict in
       assert not (TranDict.mem trans inner_dict);
       let inner_dict' = TranDict.add trans dest inner_dict in
       let trans_dict' = Type.StateSet.add orig inner_dict' trans_dict in
      (trans_dict', start, final_states))
    else
      let inner_dict = TranDict.singleton trans dest in
      let trans_dict' = Type.StateSet.add orig inner_dict trans_dict in
      (trans_dict', start, final_states)

  (* add a state to be a final state *)
  let add_final my_dfa final =
    let trans_dict, start, final_states = my_dfa in
    let final_states' = Type.StateSet.add final final_states in
    (trans_dict, start, final_states')
    

end
