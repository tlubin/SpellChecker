type tran = Correct of char | Delete | Insert | Swap

module type NFA =
sig
  type nfa_t

  (** build an nfa_t for a given input word *)
  val build: string -> nfa_t
    (* stuff for powerset construction *)
    (* add_state, ... *)

  (* takes nfa and returns start state *)
  val start_state: nfa_t -> Type.state

  (* gets next state of nfa *)
  val next_state: nfa_t -> Type.state -> tran -> Type.state

  (* get all transitions (list) leaving one state *)
  val get_transitions: nfa_t -> Type.state -> tran list

  (* bool, check if a state is a final state *)
  val is_final: nfa_t -> Type.state -> bool 


end

module Nfa : NFA =
  failwith "implement me"
