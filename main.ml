open Ranker
open Dfa
open Nfa
open Dict
open Lev
open Qwerty

module MyLev = Levenshtein (Nfa) (Dfa) (Dict)

let max_edit = 5;;

let get_word () =
  print_string "Enter sentence: ";
  read_line()

(* take in a m (match proposal) and a word and return (m, freq, prob, score) *)
let get_score ranker dict keyb m word = 
  let freq = Ranker.get_rank ranker m in
  let prob = QwertyKey.keyboard_word_match keyb m word in
  let score = (log10 (float_of_int (freq+1))) +. 10. *. prob in
  (m, freq, prob, score)

(* extract a score from a match tuple *)
let extract_score m_tuple = let _,_,_,score = m_tuple in score

let get_top_matches ranker dict keyb word =
  let rec get_matches edit_d =
    if edit_d >= max_edit then None
    else 
      let matches = MyLev.find_matches word edit_d dict in
      (* run until you find a match and go one further if you have less than 3 *)
      match matches with
	| [] -> get_matches (edit_d+1)
	| ms -> if List.length ms <= 3 && edit_d <> 0 
	  then get_matches (edit_d+1) else Some ms in
  match get_matches 0 with
    | None -> None
    | Some ms ->
      let with_info = List.map (fun m ->
	(m, Ranker.get_rank ranker m, QwertyKey.keyboard_word_match keyb m word)) ms in
      let scored =  List.map 
	(fun (str, freq, prob) -> (str, freq, prob, score freq prob)) with_info in
      Some (List.sort 
	      (fun (_,_,_,score) (_,_,_,score2) -> compare score2 score) scored)

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
    (* Build the Keyboard Map *)
    let keyboard = QwertyKey.build_map() in
    let do_action() =
      let word = get_word() in
      match get_top_matches ranker dictionary keyboard word with
	| None -> Printf.printf "That is not close to a word!\n"
	| Some ms ->
	  List.iter (fun (m, r, p, s) -> Printf.printf "%s %d %f %f\n" m r p s) ms
    in
    while true do
       do_action()
    done
;;

let main2() =
  (* If user attempts to run spellchecker directly, provide correct usage and exit *)
  if Array.length Sys.argv <> 5 then
    let usage = "Usage: Run './main.sh' to start the program." in
    (print_endline usage; exit 0)
  else
    (* Create the ranker *)
    let ranker = Ranker.create Sys.argv.(4) (int_of_string Sys.argv.(3)) in
    (* Create the dictionary *)
    let dictionary = MyLev.create_dict Sys.argv.(2) (int_of_string Sys.argv.(1)) in
    (* Build the Keyboard Map *)
    let keyboard = QwertyKey.build_map() in
    let do_action() =
      let line = get_word() in
      let words = Str.split (Str.regexp " ") line in
      let corrected = List.map (fun m -> match get_top_matches ranker dictionary keyboard m with
	| None -> m
	| Some ((w,_,_,_)::_) -> w
	| Some [] -> failwith "shouldn't happen") words in
      List.iter (fun m -> Printf.printf "%s " m) corrected;
      Printf.printf "\n"
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
