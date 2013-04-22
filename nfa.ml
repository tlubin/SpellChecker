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

  (* return the start state of an nfa *)
  let start_state my_nfa = 
    let _, start, _ = my_nfa in start

  (* get the next state given an origin and transition type *)
  let next_state my_nfa orig trans =
    let trans_dict, start, final_states = my_nfa in
    assert(Type.StateDict.mem orig trans_dict);
    let inner_dict = Type.StateDict.find orig trans_dict in
    assert(TranDict.mem trans inner_dict);
    TranDict.find trans inner_dict

  (* return a list of transitions from a given state *)
  let get_transitions my_nfa orig =
    let trans_dict, _, _ = my_nfa in 
    assert(Type.StateDict.mem orig trans_dict);
    TranDict.fold (fun tran _ trans -> tran::trans) trans_dict []

  (* return whether a given state is a final state *)
  let is_final my_nfa state =
    let _, _, final_states = my_nfa in
    Type.StateSet.mem state final_states


  let tran_types = [Delete | Insert | Swap | '$']

  (* Build NFA Helpers *)

  (* return empty nfa *)
  (*let empty = *)


  (* Build NFA Main Function *)
  let build (str: string) (edit_d: int) =
    let add_transition (transitions: Type.StateDict) (src: Type.state) (tran: tran) (dest: Type.state) =
      (* check if our starting state already exists in state dictionary *)
      (* do asserts here later for overwriting *)
      if Type.StateDict.mem src transitions then
        let inner_dict = Type.StateDict.find src transitions in
        let inner_dict = Type.TranDict.add tran dest inner_dict in
        Type.StateDict.add src inner_dict
      else
        let t_dict = Type.TranDict.singleton tran dest in
        Type.StateDict.add src t_dict
    in
    let rec build_from_string str i e t_types transitions =
      if i < String.length str then begin
        if e <= edit_d then 
          begin
            let len = String.length str in
            match t_types with
            | [] -> build_from_string str i e+1 tran_types
            | '$'::tl -> build_from_string str i e+1 tl (add_transition  (len, e) (String.get str i) (len, e+1))
            | Delete::tl -> build_from_string str i e+1 tl (add_transition transitions (i,e) Delete (i, e+1))
            | Insert::tl -> build_from_string str i e+1 tl (add_transition transitions (i,e) Insert (i+1, e+1))
            | Swap::tl -> build_from_string str i e+1 tl (add_transition transitions (i,e) Swap (i+1, e+1))
          end
        end
        build_from_string str i+1 0 tran_types transitions
      else 
        transitions
    in 
    let final_states = Type.StateSet.empty in
    let transitions = Type.StateDict.empty in
    let starting_state = (0,0) in
    build_from_string str 0 0 tran_types transitions

end
  
