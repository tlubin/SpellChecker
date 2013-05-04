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
           if word = "" then Printf.printf "%s\n" "Try again. Please input alphabetic characters."
    	   else 
             match MyLev.find_matches word dictionary with
      	       | [] -> Printf.printf "That is not close to a word!\n"
      	       | ms ->
      		 List.iter (fun (m, (r, p, s)) -> Printf.printf "%s %d %f %f\n" m r p s) ms)
      | Sentence ->
    	(let line = get_line() in
    	 let words = Str.split (Str.regexp " ") line in
    	 let corrected = List.map 
    	   (fun w -> match MyLev.find_matches w dictionary with
    	     | [] -> w
    	     | (w,_)::_ -> w) words in
    	 List.iter (fun w -> Printf.printf "%s " w) corrected;
    	 Printf.printf "\n")
    in
    while true do
       do_action()
    done
;;

main();;
