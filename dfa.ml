open Automata
open Type
open Dict

module type DFA =
sig
  (** the type for the dfa automation *)
  type t

  (** singleton returns an automata, with given start state *)
  val singleton : dfa_state -> t

  (** takes automata and returns start state *)
  val start_state : t -> dfa_state

  (** add to final states *)
  val add_final : t -> dfa_state -> t

  (** add a transition (current state, transition, new state) to the automata *)
  val add_transition : t -> dfa_state -> dfa_tran -> dfa_state -> t

  (** get the next valid string that satisfies the automation.
      None if there is no further string that satisfies it *)
  val next_valid_string : t -> string -> string option

  (** debug function to print a dfa *)
  val print_dfa : t -> unit

end

module Dfa : DFA =
struct

  (* create an automata module passing in dfa modules defined in type.ml *)
  module A = Automata (DfaStateOrderedType) (DfaStateSet) (DfaStateDict) (DfaTranDict)

  type t = A.t

  let singleton = A.singleton

  let start_state = A.start_state

  let add_final = A.add_final

  let add_transition = A.add_transition

  (* get the next state from a current state and a transtion.
     if a transition Correct l is passed, try to take it,
     otherwise try to take Other, then return None *)
  let next_state my_dfa state tran: dfa_state option =
    match A.get_transitions my_dfa state with
      | None -> None
      | Some t_dict ->
	match (DfaTranDict.mem tran t_dict, DfaTranDict.mem Other t_dict) with
	  | true, _ -> Some (DfaTranDict.find tran t_dict)
	  | false, true -> Some (DfaTranDict.find Other t_dict)
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
	let next_letter = String.get str depth in 
	let str_so_far = String.sub str 0 depth in
	let _ = stack := (str_so_far, current_state, Some next_letter)::!stack in 
	match next_state my_dfa current_state (DCorrect next_letter) with
	  | None ->
	    (* no transition so add a dummy state *)
	    let state = NfaStateSet.empty in
	    let str_so_far' = String.sub str 0 (depth+1) in
	    (* letter is None because it is just going to be backtracked anyway *)
	    let _ = stack := (str_so_far', state, None)::!stack in (!stack, state)
	  | Some s -> 
	    (* there is a transition so take it and keep going *)
	    helper s (depth+1)
    in helper (A.start_state my_dfa) 0

  (* take the first lexigraphical transition from a state and a letter.
     return the new state and the letter that was taken *)
  let first_transition my_dfa state letter: (dfa_state*char) option =
    match A.get_transitions my_dfa state with
      | None -> None
      | Some t_dict -> 
	(* take the transition corresponding to the input letter *)
	if DfaTranDict.mem (DCorrect letter) t_dict
	then Some (DfaTranDict.find (DCorrect letter) t_dict, letter)
	(* take the Other transition *)
	else if DfaTranDict.mem  (Other) t_dict
	then Some (DfaTranDict.find Other t_dict, letter)
	else
	  (* fold over dict to find the next letter transition *)
	  if letter = Dict.last_letter || DfaTranDict.cardinal t_dict = 0 then None
	  else 
	    let next_letter =
	      DfaTranDict.fold (fun tran _ next_so_far ->
		match tran with
		  | DCorrect l -> if compare l letter > 0 && compare l next_so_far < 0
		    then l else next_so_far
		  | Other -> failwith "shouldn't happen") t_dict '{' (* something after 'z' *) in
	    if next_letter = '{' then None 
	    else
	      Some (DfaTranDict.find (DCorrect next_letter) t_dict, next_letter)

  (* find the next string that satisfies the dfa from a given state and given
     letter as an outedge. None if there is no such edge *)
  let find_next_path my_dfa current_state (letter: char option) : string option =
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
	   (* if the state is final we are done, otherwise continue *)
	   if A.is_final my_dfa s then Some new_path
	   else helper s (Dict.first_letter) new_path
     in helper current_state next_letter ""


  (* get the next valid lexigraphical string in the DFA.
     return the input string if it is valid *)
  let next_valid_string my_dfa str : string option =
    let stack, state = evaluate_dfa my_dfa str in
    if A.is_final my_dfa state then Some str
    else
      (* keep peeling back layers of the stack if necessary *)
      let rec wall_search stack : string option =
	match stack with
	  | [] -> None
	  | (str_so_far, state, next_letter)::tl ->
	    match find_next_path my_dfa state next_letter with
	      | None -> wall_search tl
	      | Some path -> Some (str_so_far ^ path) in
      wall_search stack

  let print_dfa = A.print_automata

end
