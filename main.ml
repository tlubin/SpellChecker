open Dfa
open Nfa
open Dict
open Lev

module MyLev = Levenshtein (Nfa) (Dfa) (Dict)

let main () =
  (* If user attempts to run spellchecker directly, provide correct usage and exit *)
  if Array.length Sys.argv <> 4 then
    let usage = "Usage: Run './main.sh' to start the program." in
    (print_endline usage; exit 0)
  else

  (* path to the dictionary *)
  let myDict = MyLev.create_dict Sys.argv.(1) in
  (* function that asks user for valid edit distance between 0 and 3 *)
  let rec get_edit_d () : int =
    print_string "Please provide your edit distance: ";
    try
      let edit_d = int_of_string (read_line ()) in
      if (edit_d < 0 || edit_d > 3)
      then failwith "invalid index";
      edit_d
    with Failure "int_of_string" | Failure "invalid index" -> (
      print_endline ("Please provide a valid integer between 0 and 3!");
      get_edit_d ()
    ) in
  (* gets edit distance *)
  let edit_d = get_edit_d () in
  (* function that asks user for word and prints words within edit_d *)
  let do_action () =
    print_string "Enter word: ";
    let word = read_line () in
    let matches = MyLev.find_matches word edit_d myDict in
    List.iter (fun x -> print_endline x) matches in

  while true do
    do_action ()
  done
;;

main();;
