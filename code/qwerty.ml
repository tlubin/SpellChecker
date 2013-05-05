module type KEYBOARD =
sig
  type t
  (* create qwerty map *)
  val build_map : unit -> t
  val char_dist: t -> char -> char -> int
  val word_dist: t -> string -> string -> int
  val word_match: t -> string -> string -> float

  val unit_tests : string -> string -> unit
end

module QwertyKey : KEYBOARD =
struct

  (* dictionary mapping character to tuple of keyboard location *)
  module KeyboardMap = Map.Make(
    struct
      type t = char
      let compare = compare
    end)

  (* this is the type of the (KeyboardMap, max_distance) 
   * KeyboardMap maps ASCII values to (x,y) locations
   *)
  type t = (int*int) KeyboardMap.t

  (* types to deal with overlapping two strings,
   * there can either be a hang of a letter or two
   * overlapping characters. a comparison of two
   * strings is defined to be a list of these comps *)
  type comp = Overlap of char*char | Hang of char
  type comparison = comp list

  (* iterator type used to overlap two strings. stored
   * values are:
   * index of str1, length s1, s1
   * index of str2, lengh s2, s2 *)
  type overlap_iter = (int*int*string)*(int*int*string)

  (* returns max distance of the QWERTY character map *)
  let max_dist = 9

  (* penalty used for insertion/deletion *)
  let penalty = max_dist / 2

  let build_map () =
    let kmap = ref KeyboardMap.empty in
    let traverser longstring row = 
      let str_len = String.length longstring in
      let col = ref 0 in
      while (!col < str_len) do
        kmap := KeyboardMap.add (String.get longstring !col) (row, !col) !kmap;
        col := !col + 1
      done
    in
    let row1 = "qwertyuiop" in
    let row2 = "asdfghjkl" in
    let row3 = "zxcvbnm" in
    traverser row1 0;
    traverser row2 1;
    traverser row3 2;
    (!kmap)

  (*
      This function computes the "distance" between the two characters passed
      on a qwerty keyboard. This is represented by the max of dx and dy
   *)
  let char_dist kmap c1 c2 =
    let x1,y1 = KeyboardMap.find c1 kmap in
    let x2,y2 = KeyboardMap.find c2 kmap in
    let dx = abs (x1 - x2) in
    let dy = abs (y1 - y2) in
    max dx dy

  (* generate an iterator type from two strings *)
  let gen_iter str1 str2 = 
    let l1 = String.length str1 in
    let l2 = String.length str2 in
    ((0,l1, str1),(l2-1, l2, str2))

  (* passed in an iter (which has all the info about the strings being overlapped)
   * and return the new iter state along with the number of correct overlaps and
   * the string comparison type. returns None when the iterator has finished *)
  let get_overlap (iter : overlap_iter) : (overlap_iter*int*comparison) option =
    (* objects that house all the info for each string *)
    let (i1,l1,s1),(i2,l2,s2) = iter in
    if i1 = l1 then None
    else
      let comps_r = ref [] in
      let char_matches = ref 0 in
      let add_hang_left () =
	(* Hangs for string s from index 0 to index i-1 *)
	let add_left s i = 
	  String.iter (fun c -> comps_r := (Hang c)::!comps_r) (String.sub s 0 i) in
	if i1 = 0 && i2 = 0 then
	  ()
	else if i2 > 0 then
	  add_left s2 i2
	else
	  add_left s1 i1
      in add_hang_left();
      let add_overlap () =
	(* start at s1[i1] matched with s2[i2] and step forward until you hit
	   end of string return these indices*)
	let rec helper i j : int*int =
	  if i < l1 && j < l2 then
	    let c1 = String.get s1 i in
	    let c2 = String.get s2 j in
	    comps_r := (Overlap (c1,c2))::!comps_r;
	    if c1 = c2 then incr char_matches;
	    helper (i+1) (j+1)
	  else (i,j)
	in helper i1 i2
      in 
      let i_end, j_end = add_overlap() in
      let add_hang_right () =
	let add_right s i =
	  String.iter (fun c -> comps_r := (Hang c)::!comps_r) 
	    (String.sub s i ((String.length s) -i)) in
	(* add a hang for whichever (if any) string still has characters not consumed *)
	if i_end >= l1 && j_end >= l2 then
	  ()
	else if i_end < l1 then
	  add_right s1 i_end
	else
	  add_right s2 j_end
      in add_hang_right();
      (* move forward the iterator *)
      let i1' = (if i2 > 0 then i1 else i1 + 1) in
      let i2' = (if i2 > 0 then i2-1 else i2) in
      let iter' = ((i1', l1, s1),(i2', l2, s2)) in
      Some (iter', !char_matches, !comps_r)

  (* calculate distance given a comparison type *)
  let get_dist_comps kmap (comps : comparison) =
    let rec helper comps dist_so_far =
      match comps with
	| [] -> dist_so_far
	| (Overlap (c1,c2))::tl ->
	  let dist = char_dist kmap c1 c2 in
	  helper tl (dist + dist_so_far)
	| (Hang _)::tl -> helper tl (dist_so_far + (penalty/2))
    in helper comps 0

  (* calculate distance given two strings *)
  let get_dist_strings kmap s1 s2 =
    let rec helper s1 s2 dist =
      let l1 = String.length s1 in
      let l2 = String.length s2 in
      if l1 = 0 && l2 = 0 then
	dist
      else if l1 > 0 && l2 = 0 then
	dist + penalty * l1
      else if l1 = 0 && l2 > 0 then
	dist + penalty * l2
      else
	let c1 = String.get s1 0 in
	let c2 = String.get s2 0 in
	let sub1 = String.sub s1 1 (l1-1) in 
	let sub2 = String.sub s2 1 (l2-1) in
	let char_score = char_dist kmap c1 c2 in
	helper sub1 sub2 (dist + char_score)
    in helper s1 s2 0

  (* find max overlapping sequences for two strings *)
  let get_max_overlap str1 str2 : comparison =
    let rec helper (iter : overlap_iter) (best_overlap: int)
	(best_comp: comparison)  : comparison =
      match get_overlap iter with
	| None -> best_comp
	| Some (iter', overlap, comp) -> 
	  if overlap > best_overlap then helper iter' overlap comp
	  else helper iter' best_overlap best_comp
    in helper (gen_iter str1 str2) (-1) []

  (* calculate word distance by splitting up each string in half
   * if the strings are long enough and get distances using a 
   * max overlapping algorithm *)
  let word_dist kmap s1 s2 =
    let l1 = String.length s1 in
    let l2 = String.length s2 in
    if min l1 l2 < 4 then 
      get_dist_comps kmap (get_max_overlap s1 s2)
    else
      (* split both strings into halves *)
      let s11 = Str.string_before s1 (l1/2) in
      let s12 = Str.string_after s1 (l1/2) in
      let s21 = Str.string_before s2 (l2/2) in
      let s22 = Str.string_after s2 (l2/2) in
      let dist1 = get_dist_comps kmap (get_max_overlap s11 s21) in
      let dist2 = get_dist_comps kmap (get_max_overlap s12 s22) in
      dist1 + dist2

  (*
   *  The probability of a match is:

      Pr = 1 - ( D / (L * M) )

      Where D is the distance between the two strings, L is the length of
      the longer string, and M is the maximum character distance.
   *)
  let word_match kmap s1 s2 =
    let l1 = String.length s1 in
    let l2 = String.length s2 in
    let max_l = float_of_int (max l1 l2) in
    1. -. (float_of_int (word_dist kmap s1 s2)) /. 
      (max_l *. (float_of_int max_dist))

  let unit_tests s1 s2 =
    let kmap = build_map () in
    Printf.printf "%d" (word_dist kmap s1 s2)
end
