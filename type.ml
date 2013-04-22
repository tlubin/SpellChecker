type state = int*int

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

module DfaTranDict = Map.Make(
struct
  type t = Dfa.tran
  let compare a b = compare a b
end)