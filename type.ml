(* letters consumed * number of edits *)
type state = int*int

(* transitions dictionary * start state * final states *)
type automata = Type.StateDict * Type.state * Type.StateSet

module StateSet = Set.Make(
struct
  type t = int*int
  let compare a b = compare a b
end)

module StateDict = Map.Make(
struct
  type t = state
  let compare a b = compare a b
end)

module NfaTranDict = Map.Make(
struct
  type t = Nfa.tran
  let compare a b = compare a b
end)

