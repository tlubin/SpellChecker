open Score
open Dfa
open Nfa
open Dict
open Lev

module MyLev = Levenshtein (Nfa) (Dfa) (Dict)

let max_edit = 5;;

type mode = Word | Sentence | File;;

(** prepare a string by stripping bad characters, & lowercasing *)
let prepare str = 
  let stripped = Str.global_replace (Str.regexp "[^a-zA-Z ]") "" str in
  String.lowercase stripped

(** ask for and get an input word *)
let get_word () =
  print_string "Enter word: ";
  let input = String.trim (read_line()) in
  prepare input

(** ask for and get an input sentence *)
let get_line () =
  print_string "Enter sentence: ";
  let input = String.trim (read_line()) in
  prepare input

(** ask for and get an input from file *)
let get_file () = 
  print_string "Enter absolute or relative path for file: ";
  let filepath = String.trim (read_line()) in
  let input_ch = open_in filepath in
  let lines = ref [] in
  try
    while true do
      let clean_line = prepare (String.trim (input_line input_ch)) in
      lines := clean_line :: !lines
    done;
    !lines
  with End_of_file ->
    close_in input_ch;
    List.rev !lines


(** ask for and get an edit distance *)
let get_editd () =
  print_string "Enter edit distance: ";
  read_int()

(** Get the current operation mode. *)
let get_mode () =
  let rec get_mode_helper () : mode = 
    print_string "Would you like to enter word mode (W), sentence mode (S), or file mode (F)? : ";
    let x = String.trim (String.lowercase (read_line ())) in
    match x with
    | "w" -> Word 
    | "s" -> Sentence
    | "f" -> File
    | _ -> (
      print_string "That is not a valid choice. Please try again. \n";
      get_mode_helper ()
    )
  in get_mode_helper ()
  
(** helper function to cut down a list of "suggestions" to a certain size *)
let rec cut_down threshold lst =
  if threshold <= 0 then []
  else
    match lst with
    | [] -> []
    | (sug,(_,_,_))::tl -> sug :: (cut_down (threshold - 1) tl)

(** main event loop for our program *)
let main () =
  (* If user attempts to run spellchecker directly, provide correct usage and exit *)
  if Array.length Sys.argv <> 5 then
    let usage = "Usage: Run './main.sh' to start the program." in
    (print_endline usage; exit 0)
  else
    (* Create the dictionary *)
    let dictionary = Dict.create Sys.argv.(2) (int_of_string Sys.argv.(1)) in
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
      | File ->
        (let line_arr = get_file() in
          List.iteri (fun line_num line ->
            let words = Str.split (Str.regexp " ") line in
            List.iteri (fun word_num wd -> 
	      if Dict.in_dict dictionary wd then ()
	      else 
		match MyLev.find_matches wd dictionary with
		  | [] ->
		    (* no suggestions for misspelled word *)
		    (Printf.printf "(%d,%d): %s\t" line_num word_num wd;
		     Printf.printf "Unable to find a correction\n")
		  | lst -> 
                    (let lst = cut_down 3 lst in
		    (* print correction and location in file *)
                    Printf.printf "(%d,%d): %s\t" line_num word_num wd;
                    Printf.printf "%s\n" (String.concat ", " lst))
            ) words
          ) line_arr
        )
    in
    while true do
       do_action()
    done
;;

main();;
