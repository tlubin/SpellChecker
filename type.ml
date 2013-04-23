(*
 * type definitions for automata
 *)

(* letters consumed * number of edits *)
type state = int*int

(* types of transitions for nfa and dfa *)
type nfa_tran = Correct of char | Delete | Insert | Swap
type dfa_tran = Correct of char | Other

(* Set of states *)
module StateSet = Set.Make(
struct
  type t = int*int
  let compare a b = compare a b
end)

(* dictionary with states as keys *)
module StateDict = Map.Make(
struct
  type t = state
  let compare a b = compare a b
end)


(* dictionary with nfa_tran as keys *)
module NTranDict = Map.Make(
struct
  type t = nfa_tran
  let compare a b = compare a b
end)

(* dictionary with dfa_tran as keys *)
module DTranDict = Map.Make(
struct
  type t = dfa_tran
  let compare a b = compare a b
end)

(* type of dictionary with key nfa_tran and value state *)
type n_inner = state NTranDict.t

(* type of dictionary with key dfa_tran and value state *)
type d_inner = state DTranDict.t

(* types for nfa and dfa *)
type n_automata = (n_inner StateDict.t * state * StateSet.t)
type d_automata = (d_inner StateDict.t * state * StateSet.t)
