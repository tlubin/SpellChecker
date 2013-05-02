(*
 * type definitions for automata
 *)

(* types of transitions for nfa and dfa *)
type nfa_tran = NCorrect of char | Anyi | Anys | Epsilon
type dfa_tran = DCorrect of char | Other

let nfa_tran_list = [NCorrect '$'; Anyi; Anys; Epsilon]

type nfa_state = int*int

module type OrderedType = 
sig
  type t
  val compare: t -> t -> int
end

module NfaStateOrderedType =
struct
  type t = nfa_state
  let compare a b = compare a b  
end

module NfaStateSet = Set.Make(NfaStateOrderedType)

type dfa_state = NfaStateSet.t

module DfaStateOrderedType =
struct
  type t = dfa_state
  let compare a b = compare a b
end

module DfaStateSet = Set.Make(DfaStateOrderedType)

module NfaTranDict = Map.Make(
  struct
    type t = nfa_tran
    let compare a b = compare a b
  end)

module DfaTranDict = Map.Make(
  struct
    type t = dfa_tran
    let compare a b = compare a b
  end)

type nfa_tran_dict = nfa_state NfaTranDict.t
type dfa_tran_dict = dfa_state DfaTranDict.t

module NfaStateDict = Map.Make(
  struct
    type t = nfa_state
    let compare = compare
  end)

module DfaStateDict = Map.Make(
  struct
    type t = dfa_state
    let compare = compare
  end)

module KeyboardMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end
)

module type STATE =
sig

  (* type of the state *)
  type t
  type tran

  (* starting state *)
  val start_state : unit -> t

  (* string representations *)
  val string_of_state : t -> string
  val string_of_tran : tran -> string
end

(*module MyNfaState : STATE with type t = int*int with type tran = nfa_tran
  with type tran_dict = nfa_tran_dict =
struct
  type t = nfa_state
  type tran = nfa_tran
  type tran_dict = nfa_tran_dict

  let start_state () = (0,0)

  let list_of_trans () =
    [NCorrect '$'; Anyi; Anys; Epsilon]

  let string_of_state (s1, s2) = 
    Printf.sprintf "(%d, %d)" s1 s2

  let string_of_tran t =
    (match t with
      | NCorrect c -> String.make 1 c
      | Epsilon -> "Delete"
      | Anyi -> "Insert"
      | Anys -> "Swap")
end

module MyDfaState : STATE with type t = dfa_state with type tran = dfa_tran
  with type tran_dict = dfa_tran_dict =
struct
  type t = dfa_state
  type tran = dfa_tran
  type tran_dict = dfa_tran_dict

  (* starting state *)
  let start_state () = NfaStateSet.empty

  let list_of_trans () =
    [DCorrect '$'; Other]

  (* string representations *)
  let string_of_state set = 
    let string_of_tuple (s1, s2) =
      Printf.sprintf "(%d, %d)" s1 s2 in
    let str = NfaStateSet.fold (fun s str -> str ^ (string_of_tuple s) ^ ", ") set "" in
    (* remove last comma *)
    String.sub str 0 (String.rindex_from str (String.length str - 1) ',')

  let string_of_tran t =
    match t with
      | DCorrect c -> String.make 1 c
      | Other -> "Other" 

end
*)
