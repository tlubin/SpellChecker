open Type

module type AUTOMATA =
sig

  (** abstract types that define the automation *)
  type t
  type state
  type tran
  type tran_dict

  (* METHODS TO BUILD AUTOMATA *)

  (** singleton returns an automata, with given start state *)
  val singleton: state -> t

  (** add to final states *)
  val add_final: t -> state -> t 

  (** add a transition (current state, transition, new state) to the automata *)
  val add_transition: t -> state -> tran -> state -> t

  (* METHODS TO INTERACT WITH AN AUTOMATA *)

  (** takes automata and returns start state *)
  val start_state: t -> state

  (** gets next state of automata given a state and a transition. return None
     when there is no such transition *)
  val next_state: t -> state -> tran -> state option

  (** get all transitions leaving one state. returns the transition dictionary
     for efficiency so the states reached by those transitions can be found *)
  val get_transitions: t -> state -> tran_dict option

  (** check if a state is a final state *)
  val is_final: t -> state -> bool 

  (** debug function to print automata *)
  val print_automata: t -> unit

end

module Automata (StateType: OrderedType) 
  (StateSet: Set.S with type elt = StateType.t) 
  (StateDict: Map.S with type key = StateType.t) 
  (TranDict: Map.S with type key = StateType.tran) : AUTOMATA 
  with type state = StateType.t
    with type tran = StateType.tran 
      with type tran_dict = StateType.t TranDict.t =
struct
  (* define the abstract types in terms of the passed in modules *)
  type state = StateType.t
  type tran = StateType.tran
  type tran_dict = state TranDict.t

  (* an automation type is a three tuple of the following components:
     1) a dictionary mapping states to dictionaries that map tranitions to states
        (can be thought of as a dictionary with two keys, a state and a transition
        being mapped to a new state)
     2) the starting state of the automation
     3) the set of final states of the automation *)
  type t = (tran_dict StateDict.t * state * StateSet.t)    

  let singleton state = 
    (StateDict.singleton state (TranDict.empty), state, StateSet.empty)

  let add_final a final =
    let trans_dict, start, final_states = a in
    let final_states' = StateSet.add final final_states in
    (trans_dict, start, final_states')

  let add_transition a orig tran dest =
    let trans_dict, start, final_states = a in
    if StateDict.mem orig trans_dict then
      let inner_dict = StateDict.find orig trans_dict in
      let inner_dict' = TranDict.add tran dest inner_dict in
      let trans_dict' = StateDict.add orig inner_dict' trans_dict in
      (trans_dict', start, final_states)
    else
      let inner_dict = TranDict.singleton tran dest in
      let trans_dict' = StateDict.add orig inner_dict trans_dict in
      (trans_dict', start, final_states)

  let start_state a = 
    let _, start, _ = a in start

  let next_state a orig tran =
    let trans_dict, start, final_states = a in
    if not (StateDict.mem orig trans_dict) then None 
    else 
      let inner_dict = StateDict.find orig trans_dict in
      if not (TranDict.mem tran inner_dict) then None
      else Some (TranDict.find tran inner_dict)

  let get_transitions a orig =
    let trans_dict, _, _ = a in 
    if not (StateDict.mem orig trans_dict) then None
    else Some (StateDict.find orig trans_dict)

  let is_final a state =
    let _, _, final_states = a in
    StateSet.mem state final_states

  let print_automata a =
    let string_of_state_set s_set =
      let str = StateSet.fold 
	(fun s str -> str ^ (StateType.string_of_state s) ^ ", ") s_set "" in
      (* remove last comma *)
      Str.string_before str (String.length str - 1) in
    let string_of_trans_dict d =
      let string_of_inner i =
	TranDict.fold (fun k v str ->
	  str ^ "   " ^ (StateType.string_of_tran k) ^ " --> " 
	  ^ (StateType.string_of_state v) ^ "\n") i "" in
      StateDict.fold (fun k v str ->
	str ^ (StateType.string_of_state k) ^ " -->\n" ^ (string_of_inner v) ^ "\n"
      ) d "" in
    let trans_dict, start, final_states = a in
    print_string ("Start State: " ^ (StateType.string_of_state start));
    print_newline ();
    print_string ("Final States: " ^ (string_of_state_set final_states));
    print_newline ();
    print_string "--------------------------------\n";
    print_string (string_of_trans_dict trans_dict)

end
