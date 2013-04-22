type tran = Correct of char | Delete | Insert | Swap

module TranDict = Map.Make(
struct
  type t = Nfa.tran
  let compare a b = compare a b
end)

module type NFA =
sig
  type nfa_t

  (** build an nfa_t for a given input word and edit distance *)
  val build: string -> int -> nfa_t
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
struct
  (* transitions dictionary * starting state * final states *)
  type nfa_t = Type.automata

  let tran_types = [Delete | Insert | Swap]

  (* Build NFA Helpers *)

  (* return empty nfa *)
  (*let empty = *)


  (* Build NFA Main Function *)
  let build (str: string) (d: int) =
    let add_transition (transitions: Type.StateDict) (src: Type.state) (tran: tran) (dest: Type.state) =
      (* check if our starting state already exists in state dictionary *)
      (* do asserts here later for overwriting *)
      if Type.StateDict.mem src transitions then
        let inner_dict = Type.StateDict.find src transitions in
        let inner_dict = Type.TranDict.add tran dest inner_dict in
        Type.StateDict.add src inner_dict
      else
        let t_dict = Type.TranDict.singleton tran dest in
        Type.StateDict.add tran dest 
        
    in
    let expand states = 
    let final_states = Type.StateSet.empty in
    let transitions = Type.StateDict.empty in
    let starting_state = (0,0) in

    failwith "implement build"


end
  
