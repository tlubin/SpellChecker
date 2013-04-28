open Dfa
open Nfa
open Dict
open Lev

module MyLev = Levenshtein (Nfa) (Dfa) (Dict)

let usage = "Usage: ./spellchecker dictpath dictcount word edit_d\n" ^
    "wc -l dictpath to get the dictcount\n"

let main () =
  if Array.length Sys.argv <> 5 then 
    (print_string usage; exit 0)
  else
    
    let dict = Sys.argv.(1) in
    let word = Sys.argv.(3) in
    let edit_d = int_of_string Sys.argv.(4) in
    let matches = MyLev.find_matches_time word edit_d dict in
    Printf.printf "%d\n" (List.length matches)
(*    List.iter (fun x -> print_string (x ^ "\n")) matches *)
;;

main();;
