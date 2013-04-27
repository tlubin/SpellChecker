(*
 * type definitions for automata
 *)

(* types of transitions for nfa and dfa *)
type my_state = int*int
type nfa_tran = NCorrect of char | Anyi | Anys | Epsilon
type dfa_tran = DCorrect of char | Other

module NfaStateSet = Set.Make(
  struct 
    type t = my_state
    let compare a b = compare a b
  end)

type nfa_state = my_state
type dfa_state = NfaStateSet.t


module DfaStateSet = Set.Make(
  struct
    type t = dfa_state
    let compare a b = compare a b
  end)



module type STATE =
sig

  (* type of the state *)
  type t
  type tran

  (* return a list of possible transitions *)
  val list_of_trans : unit -> tran list

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

module MyDfaState : STATE with type t = NfaStateSet.t with type tran = dfa_tran =
struct
  type t = NfaStateSet.t
  type tran = dfa_tran

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
