open Score
open Dfa
open Nfa
open Dict
open Lev

module MyLev = Levenshtein (Nfa) (Dfa) (Dict)

let max_edit = 5

let readme_path = "README.md"

type mode = Word | Sentence | File | Help

type input = Menu | Input of string

(** 
  returns true if it is a string that should
  be skipped and does not need a match *)
let do_skip str = Str.string_match (Str.regexp ".*-.*") str 0

(** prepare a string line by stripping bad characters, & lowercasing *)
let prepare_line str = 
  let stripped = Str.global_replace (Str.regexp "[^a-zA-Z- ]") "" str in
  let stripped = Str.global_replace (Str.regexp "[ ]+") " " stripped in
  String.lowercase stripped

(** prepare a string word *)
let prepare_word str = 
  let stripped = Str.global_replace (Str.regexp "[^a-zA-Z- ]") "" str in
  let stripped = 
    (if (Str.string_match (Str.regexp "[a-zA-Z-]* ") stripped 0) then 
      Str.matched_string stripped
    else stripped
    )
  in
  String.trim (String.lowercase stripped)

(** ask for and get an input word *)
let get_word () : input =
  print_string "Enter word: ";
  let input = String.trim (read_line()) in
  if input = "menu()" then Menu else Input(prepare_word input)

(** ask for and get an input sentence *)
let get_line () : input =
  print_string "Enter sentence: ";
  let input = String.trim (read_line()) in
  if input = "menu()" then Menu else Input (prepare_line input)

(** ask for and get an input from file *)
let get_file () : input = 
  print_string "Enter absolute or relative path for file: ";
  let filepath = String.trim (read_line()) in
  if filepath = "menu()" then Menu else Input (filepath)

(** open up a file from string and return a ((string,int,int) list) option with
    the words in the file that we can handle along with the line number and word number
    of where they appear in the file*)
let extract_from_file filepath : ((int*int*string) list) option =
  try
    let input_ch = open_in filepath in
    let words = ref [] in
    let line_num = ref 1 in
    try
      while true do
        let clean_line = prepare_line (String.trim (input_line input_ch)) in
	let words_on_line = Str.split (Str.regexp " ") clean_line in
	List.iteri (fun word_num wd ->
	  if not (do_skip wd) then 
	    words := (!line_num,(word_num+1),wd)::!words) words_on_line;
	incr line_num;
      done;
      Some !words (* never hit *)
    with End_of_file ->
      close_in input_ch;
      Some (List.rev !words)
  with Sys_error(msg) ->
    Printf.printf "%s\n\n" msg;
    None


(** ask for and get an edit distance *)
let get_editd () =
  print_string "Enter edit distance: ";
  read_int()

(** Get the current operation mode. *)
let get_mode () =
  let rec get_mode_helper () : mode = 
    Menu.print_mode_prompt();
    let x = String.trim (String.lowercase (read_line ())) in
    print_string "\n";
    match x with
    | "w" -> Word 
    | "s" -> Sentence
    | "f" -> File
    | "h" -> Help
    | "q" -> exit 0 (* quit the program *)
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
    let mode = ref (get_mode()) in
    let do_action() =
      match !mode with
      | Word -> 
    	  (let input = get_word() in
	   match input with
	     | Menu -> print_string "\n"; mode := (get_mode()); ()
	     | Input word ->
               if word = "" || do_skip word then 
		 Printf.printf "Try again. Please input alphabetic characters only.\n\n"
    	       else 
		 match MyLev.find_matches word dictionary with
      		   | [] -> Printf.printf "That is not close to a word!\n"
      		   | ms ->
      		     (List.iter (fun (m, (r, p, s)) -> Printf.printf "%s %d %.3f %.3f\n" m r p s) ms);
		     print_string "\n")
      | Sentence ->
    	(let input = get_line() in
	 match input with
	   | Menu -> print_string "\n"; mode := (get_mode()); ()
	   | Input line ->
    	     let words = Str.split (Str.regexp " ") line in
    	     let corrected = List.map 
    	       (fun w -> 
		 if do_skip w then w
		 else
		   match MyLev.find_matches w dictionary with
    		     | [] -> w
    		     | (w,_)::_ -> w
	       ) words in
    	     List.iter (fun w -> Printf.printf "%s " w) corrected;
    	     Printf.printf "\n\n")
      | File ->
        (let input = get_file() in
	 match input with
	   | Menu -> print_string "\n"; mode := (get_mode()); ()
	   | Input filename ->
	     match extract_from_file filename with
	       | None -> ()
	       | Some words ->
		 (List.iter (fun (line_num,word_num,wd) ->
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
		 ) words;
		 print_string "\n")
        )
      | Help ->
	(* print out help instructions *)
	try
	  let fp = open_in readme_path in
	  try
	    while true do
	      Printf.printf "%s\n" (input_line fp)
	    done
	  with End_of_file ->
	    close_in fp;
	    print_string "\n";
	    mode := (get_mode())
	with Sys_error _ ->
	  print_string ("Couldn't locate the README. Go to " ^ 
			    "http://github.com/tlubin/SpellChecker for more information.\n");
	  mode := (get_mode())
    in
    while true do
       do_action()
    done
;;

main();;
