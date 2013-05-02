open Ranker
open Dfa
open Nfa
open Dict
open Lev

module MyLev = Levenshtein (Nfa) (Dfa) (Dict)

let get_word () =
  print_string "Enter word: ";
  read_line()

let main () =
  (* If user attempts to run spellchecker directly, provide correct usage and exit *)
  if Array.length Sys.argv <> 5 then
    let usage = "Usage: Run './main.sh' to start the program." in
    (print_endline usage; exit 0)
  else
    (* Create the ranker *)
    let ranker = Ranker.create Sys.argv.(4) (int_of_string Sys.argv.(3)) in
    (* Create the dictionary *)
    let dictionary = MyLev.create_dict Sys.argv.(2) (int_of_string Sys.argv.(1)) in
    let do_action() =
      let word = get_word() in
      (* check to make sure all letters *)
      let rec get_matches edit_d =
	let matches = MyLev.find_matches word edit_d dictionary in
	match matches with
	  | [] -> get_matches (edit_d+1)
	  | ms -> ms in
      let matches = get_matches 0 in
      (* sort by rank *)
      let sorted = List.sort (fun a b ->
	match Ranker.get_rank ranker a, Ranker.get_rank ranker b with
	  | None, None -> 0
	  | None, Some _ -> 1
	  | Some _, None -> -1
	  | Some x, Some y -> if x > y then -1 else 1) matches in
      List.iter print_endline sorted
    in
    while true do
       do_action()
    done
;;
(*
let main2 () =
  if Array.length Sys.argv <> 5 then 
    (print_string "Usage: ./spellchecker dictpath dictcount word edit_d\n"; exit 0)
  else
    let dict = MyLev.create_dict Sys.argv.(1) in
    let word = Sys.argv.(3) in
    let edit_d = int_of_string Sys.argv.(4) in
    let matches = MyLev.find_matches word edit_d dict in
  List.iter (fun x -> print_string (x ^ "\n")) matches
;;
*)

main();;
