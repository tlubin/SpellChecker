open Type

module type AUTOMATA =
sig
  type t
  type state
  type tran
  type tran_dict

  (* METHODS TO BUILD AUTOMATA *)

  (* singleton returns an automata, with given start state or default *)
  val singleton: state -> t

  (* takes automata and returns start state *)
  val start_state: t -> state

  (* add to final states *)
  val add_final: t -> state -> t 

  (* add a transition (cur state, transition, new state) to the automata *)
  val add_transition: t -> state -> tran -> state -> t

  (* METHODS TO INTERACT WITH AN AUTOMATA *)

  (* gets next state of automata *)
  val next_state: t -> state -> tran -> state option

  (* get all transitions leaving one state *)
  val get_transitions: t -> state -> tran_dict option

  (* bool, check if a state is a final state *)
  val is_final: t -> state -> bool 

  (* debug function to print automata *)
(*  val print_automata: t -> unit
  val print_state: state -> unit *)
end

module Automata (StateOrderedType: OrderedType) 
  (StateSet: Set.S with type elt = StateOrderedType.t) 
  (StateDict: Map.S with type key = StateOrderedType.t) 
  (TranDict: Map.S) : AUTOMATA with type state = StateOrderedType.t
    with type tran = TranDict.key with type tran_dict = StateSet.elt TranDict.t =
struct

  type state = StateOrderedType.t
  type tran = TranDict.key
  type tran_dict = state TranDict.t

  (* transitions dictionary * starting state * final states *)
  type t = (tran_dict StateDict.t * state * StateSet.t)    

  (* return a singleton dfa the given state *)
  let singleton (state: state) = 
    (StateDict.singleton state (TranDict.empty), state, StateSet.empty)

(*  let singleton_default () =
    let start = State.start_state() in
    (StateDict.singleton start (TranDict.empty), start, StateSet.empty) *)

  (* return the start state of an nfa *)
  let start_state a = 
    let _, start, _ = a in start

  (* add a state to be a final state *)
  let add_final my_dfa final =
    let trans_dict, start, final_states = my_dfa in
    let final_states' = StateSet.add final final_states in
    (trans_dict, start, final_states')

  (* add a transition from orig to dest using transition type trans *)
  let add_transition a orig trans dest =
    let trans_dict, start, final_states = a in
    if StateDict.mem orig trans_dict then 
      (let inner_dict = StateDict.find orig trans_dict in
       let inner_dict' = TranDict.add trans dest inner_dict in
       let trans_dict' = StateDict.add orig inner_dict' trans_dict in
      (trans_dict', start, final_states))
    else
      let inner_dict = TranDict.singleton trans dest in
      let trans_dict' = StateDict.add orig inner_dict trans_dict in
      (trans_dict', start, final_states)

  (* get the next state given an origin and transition type *)
  let next_state (a: t) (orig: state) (trans: tran) =
    let trans_dict, start, final_states = a in
    if not (StateDict.mem orig trans_dict) then None 
    else 
      let inner_dict = StateDict.find orig trans_dict in
      if not (TranDict.mem trans inner_dict) then None
      else Some (TranDict.find trans inner_dict)

  (* return a list of transitions from a given state *)
  let get_transitions (a: t) (orig: state) =
    let trans_dict, _, _ = a in 
    if not (StateDict.mem orig trans_dict) then None
    else Some (StateDict.find orig trans_dict)

  (* return whether a given state is a final state *)
  let is_final (a: t) (state: state) =
    let _, _, final_states = a in
    StateSet.mem state final_states

(*
  let print_state s = print_string (State.string_of_state s)

  let print_automata a =
    let string_of_state_set s_set =
      let str = StateSet.fold (fun s str -> str ^ (State.string_of_state s) ^ ", ") s_set "" in
      (* remove last comma *)
      String.sub str 0 (String.rindex_from str (String.length str - 1) ',') in
    let string_of_trans_dict d =
      let string_of_inner i =
	TranDict.fold (fun k v str ->
	  str ^ "   " ^ (State.string_of_tran k) ^ " --> " ^ (State.string_of_state v) ^ "\n"
	) i "" in
      StateDict.fold (fun k v str ->
	str ^ (State.string_of_state k) ^ " -->\n" ^ (string_of_inner v) ^ "\n"
      ) d "" in
    let trans_dict, start, final_states = a in
    print_string ("Start State: " ^ (State.string_of_state start));
    print_newline ();
    print_string ("Final States: " ^ (string_of_state_set final_states));
    print_newline ();
    print_string "--------------------------------\n";
    print_string (string_of_trans_dict trans_dict)
*)

end
