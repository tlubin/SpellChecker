open Score
open Dfa
open Nfa
open Dict
open Lev

module MyLev = Levenshtein (Nfa) (Dfa) (Dict)

let max_edit = 5;;

type mode = Word | Sentence;;

(* prepare a string by stripping bad characters, & lowercasing *)
let prepare str = 
  let stripped = Str.global_replace (Str.regexp "[^a-zA-Z ]") "" str in
  String.lowercase stripped

let get_word () =
  print_string "Enter word: ";
  let input = String.trim (read_line()) in
  prepare input

let get_line () =
  print_string "Enter sentence: ";
  let input = String.trim (read_line()) in
  prepare input

let get_editd () =
  print_string "Enter edit distance: ";
  read_int()

(** Get the current operation mode. *)
let get_mode () =
  let rec get_mode_helper () : mode = 
    print_string "Would you like to enter word mode (W) or sentence mode (S)? : ";
    let x = String.trim (String.lowercase (read_line ())) in
    if x = "w" then Word 
    else if x = "s" then Sentence 
    else (
      print_string "That is not a valid choice. Please try again. \n";
      get_mode_helper ()
    )
  in get_mode_helper ()
  

let get_top_matches dict word =
  let rec get_matches edit_d =
    if edit_d >= max_edit then None
    else 
      let matches = MyLev.find_matches word edit_d dict in
      (* run until you find a match and go one further if you have less than 3 *)
      match matches with
	| [] -> get_matches (edit_d+1)
	| ms -> if List.length ms <= 3 && edit_d <> 0 && Score.all_low ms word
	  then get_matches (edit_d+1) else Some ms in
  match get_matches 0 with
    | None -> None
    | Some ms ->
      Some (Score.get_score_extra ms word)

let main () =
  (* If user attempts to run spellchecker directly, provide correct usage and exit *)
  if Array.length Sys.argv <> 5 then
    let usage = "Usage: Run './main.sh' to start the program." in
    (print_endline usage; exit 0)
  else
    (* Create the dictionary *)
    let dictionary = MyLev.create_dict Sys.argv.(2) (int_of_string Sys.argv.(1)) in
    (* Print Welcome *)
    Menu.print_header ();
    (* ask what mode to enter *)
    let mode = get_mode() in
    let do_action() =
      match mode with
      | Word -> 
    	  (let word = get_word() in
        (if word = "" then Printf.printf "%s\n" "Try again. Please input alphabetic characters."
    	    else 
          match get_top_matches dictionary word with
      	  | None -> Printf.printf "That is not close to a word!\n"
      	  | Some ms ->
      	    List.iter (fun (m, r, p, s) -> Printf.printf "%s %d %f %f\n" m r p s) ms)
    	  )
      | Sentence ->
    	  (let line = get_line() in
    	  let words = Str.split (Str.regexp " ") line in
    	  let corrected = List.map 
    	    (fun w -> match get_top_matches dictionary w with
    	      | None -> w
    	      | Some ((w,_,_,_)::_) -> w
    	      | Some [] -> failwith "shouldn't happen") words in
    	  List.iter (fun w -> Printf.printf "%s " w) corrected;
    	  Printf.printf "\n")
    in
    while true do
       do_action()
    done
;;

let main2 () =
  let dictionary = MyLev.create_dict Sys.argv.(2) (int_of_string Sys.argv.(1)) in
  let do_action() =
    let word = get_word() in
    let edit_d = get_editd() in
    let matches = MyLev.find_matches word edit_d dictionary in
    List.iter print_endline matches
  in
  while true do
      do_action()
  done
;;

(* way to get number of probes for stats *)
(*
let get_probes() =
    let dictionary = MyLev.create_dict Sys.argv.(2) (int_of_string Sys.argv.(1)) in
    let do_action() =
      let word = get_word() in
      let edit_d = get_editd() in
      let matches = MyLev.find_matches word edit_d dictionary in
      Printf.printf "%d\n" !probes;
      reset_probes()
    in
    while true do
      do_action()
    done
;;
*)

main();;
