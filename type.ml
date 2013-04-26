(*
 * type definitions for automata
 *)

module type STATE =
sig

  (* type of the state *)
  type t
  type tran

  (* return a list of possible transitions *)
  val list_of_trans : unit -> tran list

  val state_of_indices : int -> int - > t

  val transition : t -> tran -> int -> (t*tran) option
  val transition_final : t -> t*tran

  (* starting state *)
  val start_state : unit -> t

  (* string representations *)
  val string_of_state : t -> string
  val string_of_tran : tran -> string
end

module MyNfaState : STATE =
struct
  type t = int*int
  type tran =  Correct of char | Anyi | Anys | Epsilon

  let list_of_trans () =
    [Correct '$'; Anyi; Anys; Epsilon]

  let state_of_indices i j = (i,j)
  let transition_final (i,j) = (i, j+1), Anyi

  let transition s t str edit_d =
    let i,e = s in
    match t with
      | Correct _ -> (i+1, e), (Correct (String.get str i))
      | Anys -> if e < edit_d then
	  (i+1, e+1), t else None
      | Anyi -> if e < edit_d then
	  (i, e+1), t else None
      | Epsilon -> if e < edit_d then
	  (i+1, e+1), t else None

  let string_of_state (s1, s2) = 
    Printf.sprintf "(%d, %d)\n" s1 s2

  let string_of_tran t =
    (match t with
      | Actual c -> String.make 1 c
      | Delete -> "Delete"
      | Insert -> "Insert"
      | Swap -> "Swap")
end

module MyDfaState

(* letters consumed * number of edits *)
type state = int*int

(* types of transitions for nfa and dfa *)
type nfa_tran = Actual of char | Delete | Insert | Swap
type dfa_tran = Correct of char | Other

(* Set of states *)
module StateSet = Set.Make(
struct
  type t = int*int
  let compare a b = compare a b
end)

type state_set = StateSet.t

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
