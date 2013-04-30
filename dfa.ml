open Automata
open Type
open Dict

module type DFA =
sig
  type t

  (* expose whatever else is needed for to_dfa *)
  val next_valid_string : t -> string -> string option

  val singleton : dfa_state -> t
  val start_state : t -> dfa_state
  val add_final : t -> dfa_state -> t
  val add_transition : t -> dfa_state -> dfa_tran -> dfa_state -> t

  val print_dfa : t -> unit

end

module Dfa : DFA =
struct

  module A = Automata (MyDfaState)

  type t = A.t

  let singleton = A.singleton

  let start_state = A.start_state

  let add_final = A.add_final

  let add_transition = A.add_transition

  (* extract the state from an option that is known to be Some s *)
  let extract_state (state: dfa_state option) : dfa_state =
    match state with
      | Some s -> s
      | None -> failwith "bad call to extract_state"

  (* get the next state from a current state and a transtion.
     if a transition Correct l is passed, try to take Other
     before returning None*)
  let next_state my_dfa state tran: dfa_state option =
    match A.get_transitions my_dfa state with
      | [] -> None
      | ts ->
	match (List.mem tran ts, List.mem Other ts) with
	  | true, _ -> Some (extract_state (A.next_state my_dfa state tran))
	  | false, true -> Some (extract_state (A.next_state my_dfa state Other))
	  | false, false -> None
	    
  (* evaluate the dfa as far as possible given a string and return a stack and state*)
  let evaluate_dfa my_dfa str =
    (* recursively build up a stack of (str_so_far, state, next_letter) *)
    let stack = ref [] in
    let rec helper current_state depth =
      if depth = (String.length str) then 
	(* add last state *)
	let _ = stack := (str, current_state, None)::!stack in
	(!stack, current_state)
      else 
	let letter = String.get str depth in (* next letter to be consumed *)
	let str_so_far = String.sub str 0 depth in (* what has been consumed so far *)
	let _ = stack := (str_so_far, current_state, Some letter)::!stack in 
	match next_state my_dfa current_state (DCorrect letter) with
	  | None ->
	    (* no transition so add a dummy state *)
	    let state = NfaStateSet.empty in
	    let str_so_far' = String.sub str 0 (depth+1) in
	    (* letter is None because it is just going to be backtracked anyway *)
	    let _ = stack := (str_so_far', state, None)::!stack in (!stack, state)
	  | Some s -> helper s (depth+1)
    in helper (A.start_state my_dfa) 0

  (* take the first lexigraphical transition from a state and a letter
     return the new state and the letter that was taken *)
  (* THIS IS ALGORITHMICALLY NOT GOOD!! *)
  let first_transition my_dfa state letter: (dfa_state*char) option =
    let trans_list = A.get_transitions my_dfa state in
    match List.mem Other trans_list with
      | true -> 
	if List.mem (DCorrect letter) trans_list 
	then Some (extract_state (next_state my_dfa state (DCorrect letter)), letter)
	else Some (extract_state (next_state my_dfa state Other), letter)
      | false -> 
	let sorted = List.sort (fun t1 t2 -> match t1, t2 with
	  | DCorrect a, DCorrect b -> compare a b
	  | _ -> failwith "shouldn't happen 1") trans_list in
	(* find the closest transition to the letter (inclusive) if it exists *)
	try 
	  let first_tran = List.find 
	    (fun t -> 
	      match t with
		| DCorrect a -> compare letter a = 0 || compare letter a < 0
		| _ -> failwith "shouldn't happen 2") sorted in
	  let letter = (
	    match first_tran with
	      | DCorrect l -> l
	      | Other -> failwith "shouldn't happen 3") in
	  Some (extract_state (next_state my_dfa state first_tran), letter)
	with Not_found -> None


  (* find the next string that satisfies the dfa from a given state and given
     letter as an outedge. None if there is no such edge *)
  let find_next_edge my_dfa current_state (letter: char option) : string option =
    (* return None right away if the letter is last of alphabet *)
    if letter = Some (Dict.last_letter) then None 
    else
      let next_letter =
	(match letter with
	  | None -> Dict.first_letter
	  | Some l -> Dict.next_letter l) in
     let rec helper state (letter: char) (path: string) : string option =
       (* letter is the first character to try *)
       match first_transition my_dfa state letter with
	 | None -> None
	 | Some (s,l) -> 
	   let new_path = path ^ (Char.escaped l) in
	   if A.is_final my_dfa s then Some new_path
	   else helper s (Dict.first_letter) new_path
     in helper current_state next_letter ""



  (* get the next valid lexigraphical string in the DFA *)
  let next_valid_string my_dfa str : string option =
    let stack, state = evaluate_dfa my_dfa str in
    if A.is_final my_dfa state then Some str (* word is valid *) 
    else
      let rec wall_search stack : string option =
	match stack with
	  | [] -> None
	  | (str_so_far, state, next_letter)::tl ->
	    match find_next_edge my_dfa state next_letter with
	      | None -> wall_search tl
	      | Some path -> Some (str_so_far ^ path) in
      wall_search stack

  let print_dfa = A.print_automata

end
