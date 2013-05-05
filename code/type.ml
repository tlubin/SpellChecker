(*
 * type definitions for dfa and nfa
 *)

(* transition types *)
type nfa_tran = NCorrect of char | Anyi | Anys | Epsilon
type dfa_tran = DCorrect of char | Other

(* list of nfa transitions, used in building an nfa *)
let nfa_tran_list = [NCorrect '$'; Anyi; Anys; Epsilon]

(* nfa state represents (number of letters consumed, number of edits) *)
type nfa_state = int*int

(* COMMENT HERE *)
module type OrderedType = 
sig
  (* needed to pass into Map.Make, Set.Make *)
  type t
  val compare: t -> t -> int

  (* the type of the corresponding transition *)
  type tran

  (* used to print an automata *)
  val string_of_state: t -> string
  val string_of_tran: tran -> string
end

(* OrderedType for nfa states *)
module NfaStateOrderedType =
struct
  type t = nfa_state
  let compare = compare

  type tran = nfa_tran

  let string_of_state (s1, s2) = 
    Printf.sprintf "(%d, %d)" s1 s2    

  let string_of_tran t =
    (match t with
      | NCorrect c -> String.make 1 c
      | Epsilon -> "Delete"
      | Anyi -> "Insert"
      | Anys -> "Swap")
end

(* Set of nfa states *)
module NfaStateSet = Set.Make(NfaStateOrderedType)

(* a dfa state is a set of nfa states *)
type dfa_state = NfaStateSet.t

(* OrderedType for dfa states *)
module DfaStateOrderedType =
struct
  type t = dfa_state
  let compare = compare

  type tran = dfa_tran

  let string_of_state set = 
    let string_of_tuple (s1, s2) =
      Printf.sprintf "(%d, %d)" s1 s2 in
    let str = NfaStateSet.fold (fun s str -> str ^ (string_of_tuple s) ^ ", ") set "" in
    (* remove last comma *)
    Str.string_before str (String.length str - 1)

  let string_of_tran t =
    match t with
      | DCorrect c -> String.make 1 c
      | Other -> "Other" 
end

(* Set of dfa states *)
module DfaStateSet = Set.Make(DfaStateOrderedType)

(* Dictionary of nfa transitions *)
module NfaTranDict = Map.Make(
  struct
    type t = nfa_tran
    let compare a b = compare a b
  end)

(* Dictionary of dfa transitions *)
module DfaTranDict = Map.Make(
  struct
    type t = dfa_tran
    let compare a b = compare a b
  end)

(* types for dictionaries with key tran and value state *)
type nfa_tran_dict = nfa_state NfaTranDict.t
type dfa_tran_dict = dfa_state DfaTranDict.t

(* Dictionary of nfa states *)
module NfaStateDict = Map.Make(NfaStateOrderedType)

(* Dictionary of dfa states *)
module DfaStateDict = Map.Make(DfaStateOrderedType)
