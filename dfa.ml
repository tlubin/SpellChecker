type tran = Correct of char | Other

module type DFA =
sig
  type dfa_t
  
  (* return the next valid string satisfying the dfa *)
  val next_valid_string: string -> string

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

  (* get the next valid lexigraphical string in the DFA *)
  let next_valid_string current =
    failwith "todo"

  (* return a singleton dfa with the given state *)
  let singleton (state: Type.state) = Type.StateDict.singleton state (TranDict.empty)

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
