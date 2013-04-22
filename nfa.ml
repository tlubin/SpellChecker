type tran = Correct of char | Delete | Insert | Swap

module type NFA =
sig
  type nfa_t

  (** build an nfa_t for a given input word *)
  val build: string -> nfa_t
    (* stuff for powerset construction *)
    (* add_state, ... *)
end

module Nfa : NFA =
  failwith "implement me"
