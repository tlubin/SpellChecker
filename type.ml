(*
 * type definitions for automata
 *)

(* types of transitions for nfa and dfa *)
type my_state = int*int
type nfa_tran = NCorrect of char | Anyi | Anys | Epsilon
type dfa_tran = DCorrect of char | Other

module StateSet = Set.Make(
  struct 
    type t = my_state
    let compare a b = compare a b
  end)


module type STATE =
sig

  (* type of the state *)
  type t
  type tran

  (* return a list of possible transitions *)
  val list_of_trans : unit -> tran list

  val state_of_indices : int -> int -> t

  val transition : t -> tran -> string -> int -> (t*tran) option
  val transition_final : t -> t*tran

  (* starting state *)
  val start_state : unit -> t

  (* string representations *)
  val string_of_state : t -> string
  val string_of_tran : tran -> string
end

module MyNfaState : STATE with type t = int*int with type tran = nfa_tran =
struct
  type t = int*int
  type tran = nfa_tran

  let start_state () = (0,0)

  let list_of_trans () =
    [Correct '$'; Anyi; Anys; Epsilon]

  let state_of_indices i j = (i,j)
  let transition_final (i,j) = (i, j+1), Anyi

  let transition s t str edit_d =
    let i,e = s in
    match t with
      | Correct _ -> Some ((i+1, e), (Correct (String.get str i)))
      | Anys -> if e < edit_d then
	  Some ((i+1, e+1), t) else None
      | Anyi -> if e < edit_d then
	  Some ((i, e+1), t) else None
      | Epsilon -> if e < edit_d then
	  Some ((i+1, e+1), t) else None

  let string_of_state (s1, s2) = 
    Printf.sprintf "(%d, %d)\n" s1 s2

  let string_of_tran t =
    (match t with
      | Correct c -> String.make 1 c
      | Epsilon -> "Delete"
      | Anyi -> "Insert"
      | Anys -> "Swap")
end

module MyDfaState : STATE with type t = StateSet.t with type tran = dfa_tran =
struct
  type t = StateSet.t
  type tran = dfa_tran
  
end

(* letters consumed * number of edits *)
type state = int*int


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
