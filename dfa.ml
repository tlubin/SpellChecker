(* note: state names are not well defined in this module *)
open Type

module type DFA =
sig
  type dfa_t
  
  (* return the next valid string satisfying the dfa *)
  val next_valid_string: dfa_t -> string -> string option

  (* singleton returns a dfa, with given start state *)
  val singleton: state -> dfa_t

  (* add a transition (cur state, transition, new state) to the dfa *)
  val add_transition: dfa_t -> state -> dfa_tran -> state -> dfa_t

  (* add to final states *)
  val add_final: dfa_t -> state -> dfa_t 

  (* debug function to print a dfa *)
  val print_dfa: dfa_t -> unit

  (* run tests on functions *)
  val unit_tests: unit -> unit
end

module Dfa : DFA =
struct

  type dfa_t = d_automata

  (* Helper Functions for next_valid_string *)
  let start_state my_dfa =
    let _, start, _ = my_dfa in start

  let is_final my_dfa state =
    let _, _, final_states = my_dfa in StateSet.mem state final_states

  (* get the next state from a current state. try to go by transition
     tran but otherwise go by Other *)
  let next_state my_dfa state tran: state option =
    let trans_dict, _, _ = my_dfa in
    assert(StateDict.mem state trans_dict);
    let inner_dict = StateDict.find state trans_dict in
    match (DTranDict.mem tran inner_dict), (DTranDict.mem Other inner_dict) with
      | true, _ -> Some (DTranDict.find tran inner_dict)
      | false, true -> Some (DTranDict.find Other inner_dict)
      | false, false -> None

  let get_transitions my_dfa orig =
    let trans_dict, _, _ = my_dfa in
    assert(StateDict.mem orig trans_dict);
    DTranDict.fold (fun tran _ trans -> tran::trans) (StateDict.find orig trans_dict) []

  (* return the lexigraphical first transition from a given state if one exists
     from an input letter or as close to it as possible *)
  (* THIS IS ALGORITHMICALLY NOT GOOD!! *)
  let first_transition my_dfa state letter: dfa_tran option =
    let trans_list = get_transitions my_dfa state in
    match  List.mem Other trans_list with
      | true -> 
	if List.mem (Correct letter) trans_list 
	then Some (Correct letter) else Some Other
      | false -> 
	let sorted = List.sort (fun t1 t2 -> match t1, t2 with
	  | Correct a, Correct b -> compare a b
	  | _ -> failwith "shouldn't happen") trans_list in
	(* find the closest transition to the letter (inclusive) if it exists *)
	try 
	  let first_tran = List.find 
	    (fun t -> 
	      match t with
		| Correct a -> compare letter a = 0 || compare letter a < 0
		| _ -> failwith "shouldn't happen") sorted in Some first_tran
	with Not_found -> None

  let find_next_edge my_dfa current_state (letter: char option) : string option =
    let _ = print_string "checkpoint 1" in
    (* return None right away if the letter is 'z' *)
    if letter = Some 'z' then None 
    else
      let next_letter =
	(match letter with
	  | None -> 'a'
	  | Some l -> Char.chr ((Char.code l) + 1)) in
     let rec helper state (letter: char) (path: string) : string option =
       (* letter is the first character to try *)
       match first_transition my_dfa state letter with
	 | None -> None
	 | Some t -> 
	   let t_letter = (match t with
	     | Correct k -> k
	     | Other -> failwith "shouldn't happen") in
	   let new_path = path ^ (Char.escaped t_letter) in
	   let new_state = (match next_state my_dfa state t with 
	     | Some t -> t
	     | None -> failwith "shouldn't happen") in
	   if is_final my_dfa new_state then Some new_path 
	   else helper new_state 'a' new_path (* DONT HARD CODE IN THE a *)
     in helper current_state next_letter ""

  (* get the next valid lexigraphical string in the DFA *)
  (* INCOMPLETE IMPLEMENTATION *)
  let next_valid_string my_dfa str : string option =
    (* evaluate the dfa as far as possible *)
    let rec evaluate_dfa current_state depth stack =
      let letter : char option = (
	if depth = (String.length str) then None
	else Some (String.get str depth)) in (* next letter to be consumed *)
      let str_so_far = String.sub str 0 depth in (* what has been consumed so far *)
      let stack = (str_so_far, current_state, letter)::stack in (* add the current info the the stack *)
      if depth = (String.length str) then (stack, current_state)
      else
	(* PATCH FIX *)
	let c = (match letter with
	  | None -> failwith "shouldn't happen"
	  | Some k -> k) in
	match next_state my_dfa current_state (Correct c) with
	  | None ->
	    (* add a dummy state of (-1,-1) to the stack *)
	    let state = (-1, -1) in
	    let str_so_far' = String.sub str 0 (depth+1) in
	    (* letter is None because it is just going to be backtracked anyway *)
	    let stack = (str_so_far', state, None)::stack in
	    (stack, state)
	  | Some s -> evaluate_dfa s (depth+1) stack in
    let stack, state = evaluate_dfa (start_state my_dfa) 0 [] in
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
       assert(not (DTranDict.mem trans inner_dict));
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
    

  let unit_tests () =
    let dfa = ref (singleton (0,0)) in
    dfa := (add_transition !dfa (0,0) (Correct 'b') (1,1));
    dfa := (add_final !dfa (1,1));
    dfa := (add_transition !dfa (0,0) Other (2,2));
    dfa := (add_transition !dfa (2,2) (Correct 'f') (3,3));
    let _ = print_dfa !dfa in
    let next_valid = (match next_valid_string !dfa "a" with
      | Some s -> s
      | None -> "none") in
    print_string next_valid
    
end
