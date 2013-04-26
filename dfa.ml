(* note: state names are not well defined in this module *)
open Type

module type DFA =
sig
  type dfa_t
  type state
  type tran
  
  (* return the next valid string satisfying the dfa or None *)
(*  val next_valid_string: dfa_t -> string -> string option *)

  (* singleton returns a dfa, with given start state *)
  val singleton: state -> dfa_t

  (* add a transition (cur state, transition, new state) to the dfa *)
  val add_transition: dfa_t -> state -> tran -> state -> dfa_t

  (* add to final states *)
  val add_final: dfa_t -> state -> dfa_t 

  (* debug function to print a dfa *)
  val print_dfa: dfa_t -> unit

  (* is final/ next_state *)

  (* run tests on functions *)
  val unit_tests: unit -> unit
  val build_test: unit -> dfa_t

end

(* State taken in as input is a module that defines a state type.
   Dfa has "state" as a set of State.t *)
module Dfa (State: STATE) : DFA =
struct
  type state = State.t
  type tran = Other | 

  (* other helpful modules and types *)
  module StateSet = Set.Make(
    struct
      type t = state
      let compare a b = compare a b
    end)

  module StateDict = Map.Make(
    struct
      type t = state
      let compare a b = compare a b
    end)

  module TranDict = Map.Make(
    struct
      type t = tran
      let compare a b = compare a b
    end)

  type d_inner = state TranDict.t

  type dfa_t = (d_inner StateDict.t * state * StateSet.t)

  (* get the start state *)
  let start_state my_dfa =
    let _, start, _ = my_dfa in start
  
  (* check whether a state is final *)
  let is_final my_dfa state =
    let _, _, final_states = my_dfa in StateSet.mem state final_states

  (* get the next state from a current state and a transtion.
     if a transition Correct l is passed, try to take Other
     before returning None*)
  let next_state my_dfa state tran: state option =
    let trans_dict, _, _ = my_dfa in
    if not (StateDict.mem state trans_dict) then None
    else 
      let inner_dict = StateDict.find state trans_dict in
      match (TranDict.mem tran inner_dict), (TranDict.mem Other inner_dict) with
	| true, _ -> Some (TranDict.find tran inner_dict)
	| false, true -> Some (TranDict.find Other inner_dict)
	| false, false -> None
  
  (* return a list of transitions from a given state *)
  let get_transitions my_dfa orig =
    let trans_dict, _, _ = my_dfa in
    if not (StateDict.mem orig trans_dict) then []
    else TranDict.fold (fun tran _ trans -> tran::trans) 
      (StateDict.find orig trans_dict) []

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
	match next_state my_dfa current_state (Correct letter) with
	  | None ->
	    (* no transition so add a dummy state *)
	    let state = (-1, -1) in
	    let str_so_far' = String.sub str 0 (depth+1) in
	    (* letter is None because it is just going to be backtracked anyway *)
	    let _ = stack := (str_so_far', state, None)::!stack in (!stack, state)
	  | Some s -> helper s (depth+1)
    in helper (start_state my_dfa) 0

  (* extract the state from an option that is known to be Some s *)
  let extract_state (state: state option) : state =
    match state with
      | Some s -> s
      | None -> failwith "bad call to extract_state"

  (* take the first lexigraphical transition from a state and a letter
     return the new state and the letter that was taken *)
  (* THIS IS ALGORITHMICALLY NOT GOOD!! *)
  let first_transition my_dfa state letter: (state*char) option =
    let trans_list = get_transitions my_dfa state in
    match List.mem Other trans_list with
      | true -> 
	if List.mem (Correct letter) trans_list 
	then Some (extract_state (next_state my_dfa state (Correct letter)), letter)
	else Some (extract_state (next_state my_dfa state Other), letter)
      | false -> 
	let sorted = List.sort (fun t1 t2 -> match t1, t2 with
	  | Correct a, Correct b -> compare a b
	  | _ -> failwith "shouldn't happen 1") trans_list in
	(* find the closest transition to the letter (inclusive) if it exists *)
	try 
	  let first_tran = List.find 
	    (fun t -> 
	      match t with
		| Correct a -> compare letter a = 0 || compare letter a < 0
		| _ -> failwith "shouldn't happen 2") sorted in
	  let letter = (
	    match first_tran with
	      | Correct l -> l
	      | Other -> failwith "shouldn't happen 3") in
	  Some (extract_state (next_state my_dfa state first_tran), letter)
	with Not_found -> None
  
  (* find the next string that satisfies the dfa from a given state and given
     letter as an outedge. None if there is no such edge *)
  let find_next_edge my_dfa current_state (letter: char option) : string option =
    (* return None right away if the letter is last of alphabet *)
    if letter = Some (Dict.last_letter()) then None 
    else
      let next_letter =
	(match letter with
	  | None -> Dict.first_letter()
	  | Some l -> Dict.next_letter l) in
     let rec helper state (letter: char) (path: string) : string option =
       (* letter is the first character to try *)
       match first_transition my_dfa state letter with
	 | None -> None
	 | Some (s,l) -> 
	   let new_path = path ^ (Char.escaped l) in
	   if is_final my_dfa s then Some new_path
	   else helper s (Dict.first_letter()) new_path
     in helper current_state next_letter ""

  (* get the next valid lexigraphical string in the DFA *)
  let next_valid_string my_dfa str : string option =
    let stack, state = evaluate_dfa my_dfa str in
    if is_final my_dfa state then Some str (* word is valid *) 
    else
      let rec wall_search stack : string option =
	match stack with
	  | [] -> None
	  | (str_so_far, state, next_letter)::tl ->
	    match find_next_edge my_dfa state next_letter with
	      | None -> wall_search tl
	      | Some path -> Some (str_so_far ^ path) in
      wall_search stack
  
  (* return a singleton dfa with the given state *)
  let singleton (state: state) = 
    (StateDict.singleton state (DTranDict.empty), state, StateSet.empty)

  (* add a transition from orig to dest using transition type trans *)
  let add_transition my_dfa orig trans dest =
    let trans_dict, start, final_states = my_dfa in
    if StateDict.mem orig trans_dict then 
      (let inner_dict = StateDict.find orig trans_dict in
(*       assert(not (DTranDict.mem trans inner_dict)); *)
       let inner_dict' = DTranDict.add trans dest inner_dict in
       let trans_dict' = StateDict.add orig inner_dict' trans_dict in
      (trans_dict', start, final_states))
    else
      let inner_dict = DTranDict.singleton trans dest in
      let trans_dict' = StateDict.add orig inner_dict trans_dict in
      (trans_dict', start, final_states)

  (* add a state to be a final state *)
  let add_final my_dfa final =
    let trans_dict, start, final_states = my_dfa in
    let final_states' = StateSet.add final final_states in
    (trans_dict, start, final_states')
  
  (* debugging function to print a dfa *)
  let print_dfa my_dfa =
    let string_of_state (s1, s2) = Printf.sprintf "(%d, %d)" s1 s2 in
    let string_of_state_set s_set =
      let str = StateSet.fold (fun s str -> str ^ (string_of_state s) ^ ", ") s_set "" in
      (* remove last comma *)
      String.sub str 0 (String.rindex_from str (String.length str - 1) ',') in
    let string_of_tran t =
      (match t with
	| Correct c -> String.make 1 c
	| Other -> "Other") in
    let string_of_trans_dict d =
      let string_of_inner i =
	DTranDict.fold (fun k v str ->
	  str ^ "   " ^ (string_of_tran k) ^ " --> " ^ (string_of_state v) ^ "\n"
	) i "" in
      StateDict.fold (fun k v str ->
	str ^ (string_of_state k) ^ " -->\n" ^ (string_of_inner v) ^ "\n"
      ) d "" in
    let trans_dict, start, final_states = my_dfa in
    print_string ("Start State: " ^ (string_of_state start));
    print_newline ();
    print_string ("Final States: " ^ (string_of_state_set final_states));
    print_newline ();
    print_string "--------------------------------\n";
    print_string (string_of_trans_dict trans_dict)

  (* debugging. build the dfa for food with one edit *)
  let build_test () =
    let dfa = ref (singleton (0,0)) in
    dfa := (add_transition !dfa (0,0) (Correct 'f') (1,1));
    dfa := (add_transition !dfa (0,0) (Correct 'o') (2,2));
    dfa := (add_transition !dfa (0,0) Other (3,3));
    dfa := (add_transition !dfa (1,1) (Correct 'o') (4,4));
    dfa := (add_transition !dfa (1,1) Other (5,5));
    dfa := (add_transition !dfa (2,2) (Correct 'f') (6,6));
    dfa := (add_transition !dfa (2,2) (Correct 'o') (9,9));
    dfa := (add_transition !dfa (3,3) (Correct 'f') (6,6));
    dfa := (add_transition !dfa (3,3) (Correct 'o') (10,10));
    dfa := (add_transition !dfa (4,4) (Correct 'o') (7,7));
    dfa := (add_transition !dfa (4,4) (Correct 'd') (8,8));
    dfa := (add_transition !dfa (4,4) Other (9,9));
    dfa := (add_transition !dfa (5,5) (Correct 'o') (9,9));
    dfa := (add_transition !dfa (6,6) (Correct 'o') (10,10));
    dfa := (add_transition !dfa (7,7) (Correct 'd') (11,11));
    dfa := (add_transition !dfa (7,7) Other (12,12));
    dfa := (add_transition !dfa (8,8) (Correct 'o') (13,13));
    dfa := (add_transition !dfa (8,8) (Correct 'd') (14,14));
    dfa := (add_transition !dfa (9,9) (Correct 'o') (13,13));
    dfa := (add_transition !dfa (9,9) (Correct 'd') (14,14));
    dfa := (add_transition !dfa (10,10) (Correct 'o') (13,13));
    dfa := (add_transition !dfa (11,11) Other (14,14));
    dfa := (add_transition !dfa (12,12) (Correct 'd') (14,14));
    dfa := (add_transition !dfa (13,13) (Correct 'd') (14,14));
    dfa := (add_final !dfa (7,7));
    dfa := (add_final !dfa (8,8));
    dfa := (add_final !dfa (11,11));
    dfa := (add_final !dfa (12,12));
    dfa := (add_final !dfa (14,14));
    !dfa
  
  (* write tests here *)
  let unit_tests () =
    let dfa = ref (singleton (0, 0)) in
    dfa := (add_transition !dfa (0,0) (Correct 'f') (1,1));
    dfa := (add_final !dfa (1,1));
    dfa := (add_transition !dfa (0,0) Other (2,2));
    dfa := (add_transition !dfa (2,2) (Correct 'f') (3,3));
    dfa := (add_final !dfa (3,3));
    let _ = print_dfa !dfa in
    let next_valid str = (match next_valid_string !dfa str with
      | Some s -> s ^ "\n"
      | None -> "none\n") in
    print_string (next_valid "bz")
    
end
