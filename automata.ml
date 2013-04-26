module type AUTOMATA =
sig
  type t
  type state
  type tran

  (* singleton returns an automata, with given start state *)
  val singleton: state -> t

  (* add a transition (cur state, transition, new state) to the automata *)
  val add_transition: t -> state -> tran -> state -> t

  (* add to final states *)
  val add_final: t -> state -> t 



  (* takes automata and returns start state *)
  val start_state: t -> state

  (* gets next state of automata *)
  val next_state: t -> state -> tran -> state

  (* get all transitions (list) leaving one state *)
  val get_transitions: t -> state -> tran list

  (* bool, check if a state is a final state *)
  val is_final: t -> state -> bool 



  (* debug function to print automata *)
  val print_automata: t -> unit
end

(*
module Automata (State: STATE) : (AUTOMATA with type state = State.t) =
struct
  sdkljfjklsadfllak
end*)
