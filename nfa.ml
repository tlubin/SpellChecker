open Type

module type NFA =
sig
  type nfa_t

  (** build an nfa_t for a given input word and edit distance *)
  val build: string -> int -> nfa_t
    (* stuff for powerset construction *)
    (* add_state, ... *)

  (* takes nfa and returns start state *)
  val start_state: nfa_t -> state

  (* gets next state of nfa *)
  val next_state: nfa_t -> state -> nfa_tran -> state

  (* get all transitions (list) leaving one state *)
  val get_transitions: nfa_t -> state -> nfa_tran list

  (* bool, check if a state is a final state *)
  val is_final: nfa_t -> state -> bool 

  (* debug function to print a dfa *)
  val print_nfa: nfa_t -> unit
  val unit_tests: unit -> unit

end

module Nfa : NFA =
struct
  (* transitions dictionary * starting state * final states *)
  type nfa_t = n_automata

  (* return the start state of an nfa *)
  let start_state my_nfa = 
    let _, start, _ = my_nfa in start

  (* get the next state given an origin and transition type *)
  let next_state (my_nfa: n_automata) (orig: state) (trans: nfa_tran) =
    let trans_dict, start, final_states = my_nfa in
    assert(StateDict.mem orig trans_dict);
    let inner_dict = StateDict.find orig trans_dict in
    assert(NTranDict.mem trans inner_dict);
    NTranDict.find trans inner_dict

  (* return a list of transitions from a given state *)
  let get_transitions (my_nfa: n_automata) (orig: state) =
    let trans_dict, _, _ = my_nfa in 
    assert ( StateDict.mem orig trans_dict );
    let inner_dict = StateDict.find orig trans_dict in
    NTranDict.fold (fun tran _ trans -> tran::trans) (inner_dict) []

  (* return whether a given state is a final state *)
  let is_final (my_nfa: n_automata) (state: state) =
    let _, _, final_states = my_nfa in
    StateSet.mem state final_states


  (* array of transition types to use later 
   * '$' - arbitrary char
   *)
  let tran_types = [ Delete ; Insert ; Swap ; Actual '$']

  (* Build NFA Helpers *)
  let add_transition (transitions: n_inner StateDict.t) (src: state) 
    (tran: nfa_tran) (dest: state) =
    (* check if our starting state already exists in state dictionary *)
    (* do asserts here later for overwriting *)
    if StateDict.mem src transitions then
      let inner_dict = StateDict.find src transitions in
      let inner_dict = NTranDict.add tran dest inner_dict in
      StateDict.add src inner_dict transitions
    else
      let t_dict = NTranDict.singleton tran dest in
      StateDict.add src t_dict transitions

  (* Build NFA Main Function *)
let build str edit_d =
  let starting_state = (0,0) in
  let final_states = ref StateSet.empty in
  let transitions = ref (StateDict.singleton starting_state NTranDict.empty) in
  let len = String.length str in
  let add_edges () =
    let i = ref 0 in
    let e = ref 0 in
    while !i < len do
      while !e <= edit_d do
	(* add transitions from each state *)
        List.iter (fun t_type -> 
          match t_type with
            | Actual _ -> 
              transitions := (add_transition !transitions (!i, !e) (Actual (String.get str !i)) 
				(!i+1, !e))
            | Delete -> if !e < edit_d then 
                transitions := (add_transition !transitions (!i,!e) Delete (!i+1, !e+1))
            | Insert -> if !e < edit_d then 
                transitions := (add_transition !transitions (!i,!e) Insert (!i, !e+1)) 
            | Swap -> if !e < edit_d then 
                transitions := (add_transition !transitions (!i,!e) Swap (!i+1, !e+1))
        ) tran_types;
        e := !e + 1
      done;
      i := !i + 1;
      e := 0
    done in
  let add_final_states () =
    let e = ref 0 in
    while !e <= edit_d do
      final_states := (StateSet.add (len, !e) (!final_states));
      if !e < edit_d then
	transitions := (add_transition !transitions (len, !e) Insert (len, !e+1));
      e := !e + 1
    done in
  add_edges();
  add_final_states();
  (!transitions, starting_state, !final_states)

  let unit_tests () =
    failwith "implement"
    

  (* debugging function to print a dfa *)
  let print_nfa my_nfa =
    let string_of_state (s1, s2) = Printf.sprintf "(%d, %d)" s1 s2 in
    let string_of_state_set s_set =
      let str = StateSet.fold (fun s str -> str ^ (string_of_state s) ^ ", ") s_set "" in
      (* remove last comma *)
      String.sub str 0 (String.rindex_from str (String.length str - 1) ',') in
    let string_of_tran t =
      (match t with
	| Actual c -> String.make 1 c
	| Delete -> "Delete"
	| Insert -> "Insert"
	| Swap -> "Swap") in
    let string_of_trans_dict d =
      let string_of_inner i =
	NTranDict.fold (fun k v str ->
	  str ^ "   " ^ (string_of_tran k) ^ " --> " ^ (string_of_state v) ^ "\n"
	) i "" in
      StateDict.fold (fun k v str ->
	str ^ (string_of_state k) ^ " -->\n" ^ (string_of_inner v) ^ "\n"
      ) d "" in
    let trans_dict, start, final_states = my_nfa in
    print_string ("Start State: " ^ (string_of_state start));
    print_newline ();
    print_string ("Final States: " ^ (string_of_state_set final_states));
    print_newline ();
    print_string "--------------------------------\n";
    print_string (string_of_trans_dict trans_dict)

end
