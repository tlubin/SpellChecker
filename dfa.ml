type tran = Correct of char | Other

module type DFA =
sig
  type dfa_t
  
  (** return the next valid string satisfying the dfa *)
  val next_valid_string: string -> string

    (* stuff needed to build the dfa. i.e. add_transition,... *)
  
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

  (* return a singleton dfa with the given state *)
  let singleton (state: Type.state) = Type.StateDict.singleton state (TranDict.empty)

  let add_transition my_dfa orig trans dest =
    

end
