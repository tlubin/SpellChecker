open Type

(*module state_t : (Map.OrderedType with type t=state) =
  type t = state
  let compare s1 s2 = 
    cmp s1 s2
*)

module nfa = Map.Make(state_t);;
