open Type

module type NFA =
sig
  type nfa_t
  type state
  type tran

  (** build an nfa_t for a given input word and edit distance *)
  val build: string -> int -> nfa_t

  (* takes nfa and returns start state *)
  val start_state: nfa_t -> state

  (* gets next state of nfa *)
  val next_state: nfa_t -> state -> tran -> state

  (* get all transitions (list) leaving one state *)
  val get_transitions: nfa_t -> state -> tran list

  (* bool, check if a state is a final state *)
  val is_final: nfa_t -> state -> bool 

  (* debug function to print a dfa *)
  val print_state : state -> unit
  val print_trans : tran list -> unit
  val print_nfa: nfa_t -> unit
  val unit_tests: unit -> unit

end

module Nfa (State: STATE) : NFA =
struct

  type state = State.t
  type tran = State.tran

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

  type n_inner = state TranDict.t

  (* transitions dictionary * starting state * final states *)
  type nfa_t = (n_inner StateDict.t * state * StateSet.t)


  (* return the start state of an nfa *)
  let start_state my_nfa = 
    let _, start, _ = my_nfa in start

  (* get the next state given an origin and transition type *)
  let next_state (my_nfa: nfa_t) (orig: state) (trans: tran) =
    let trans_dict, start, final_states = my_nfa in
    assert(StateDict.mem orig trans_dict);
    let inner_dict = StateDict.find orig trans_dict in
    assert(TranDict.mem trans inner_dict);
    TranDict.find trans inner_dict

  (* return a list of transitions from a given state *)
  let get_transitions (my_nfa: nfa_t) (orig: state) =
    let trans_dict, _, _ = my_nfa in 
    if not (StateDict.mem orig trans_dict) then []
    else TranDict.fold (fun tran _ trans -> tran::trans) 
      (StateDict.find orig trans_dict) []

  (* return whether a given state is a final state *)
  let is_final (my_nfa: nfa_t) (state: state) =
    let _, _, final_states = my_nfa in
    StateSet.mem state final_states


  (* array of transition types to use later 
   * '$' - arbitrary char
   *)
  let tran_types = State.list_of_trans()

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
  let starting_state = State.start_state in
  let final_states = ref StateSet.empty in
  let transitions = ref (StateDict.singleton starting_state TranDict.empty) in
  let len = String.length str in
  let add_edges () =
    let i = ref 0 in
    let e = ref 0 in
    while !i < len do
      while !e <= edit_d do
	(* add transitions from each state *)
        List.iter (fun t -> 
	  let current = (State.state_of_indices !i !e) in
	  match State.transition current t str edit_d with
	    | None -> ()
	    | Some (st,tr) -> transitions := add_transition !transitions current tr st)
	State.list_of_trans()
        e := !e + 1
      done;
      i := !i + 1;
      e := 0
    done in
  let add_final_states () =
    let e = ref 0 in
    while !e <= edit_d do
      final_states := (StateSet.add (State.state_of_indices len !e) (!final_states));
      if !e < edit_d then
	let current = (State.state_of_indices len !e) in
	let st, tr = State.transition_final current in
	transitions := (add_transition !transitions current tr st)
      e := !e + 1
    done in
  add_edges();
  add_final_states();
  (!transitions, starting_state, !final_states)

  let unit_tests () =
    failwith "implement"
    

  (* debugging function to print a dfa *)
  let string_of_state (s1, s2) = Printf.sprintf "(%d, %d)\n" s1 s2

  let string_of_tran t =
    (match t with
      | Actual c -> String.make 1 c
      | Delete -> "Delete"
      | Insert -> "Insert"
      | Swap -> "Swap") ^ "\n"

  let print_state s = print_string (string_of_state s)
    
  let print_trans ts = 
    print_string (List.fold_left (fun str t -> str ^ (string_of_tran t) ^ ", ")
      "" ts)

  let print_nfa my_nfa =
    let string_of_state_set s_set =
      let str = StateSet.fold (fun s str -> str ^ (string_of_state s) ^ ", ") s_set "" in
      (* remove last comma *)
      String.sub str 0 (String.rindex_from str (String.length str - 1) ',') in
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
