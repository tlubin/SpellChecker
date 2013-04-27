open Dfa
open Nfa
open Dict
open Lev

(* this is going to be the main file for the command line thing *)
module MyLev = Levenshtein (Nfa) (Dfa) (Dict);;

let dict = Sys.argv.(1);;
let matches = MyLev.find_matches "nice" 1 dict;;
List.iter print_string matches;;
