open Type


module type KEYBOARD =
sig
  type t
  (* create qwerty map *)
  val build_map : unit -> t
  val keyboard_char_distance: t -> char -> char -> int
  val keyboard_word_distance: t -> string -> string -> int
  val keyboard_word_match: t -> string -> string -> float

  val unit_tests : unit -> unit
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

  (* returns max distance of the QWERTY character map *)
  let keyboard_max_distance = 9

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
  let keyboard_char_distance kmap c1 c2 =
    let x1,y1 = KeyboardMap.find c1 kmap in
    let x2,y2 = KeyboardMap.find c2 kmap in
    let dx = abs (x1 - x2) in
    let dy = abs (y1 - y2) in
    max dx dy

  (* 
   *  Returns the sum of the distances between corresponding characters
      in the two strings.  If one string is longer than the other the 
      remaining characters are counted as having the same value as the
      maximum distance.
   *)
  let rec keyboard_word_distance kmap s1 s2 =
    let rec helper s1 s2 dist =
      let l1 = String.length s1 in
      let l2 = String.length s2 in
      if l1 = 0 && l2 = 0 then
	dist
      else if l1 > 0 && l2 = 0 then
	dist + keyboard_max_distance * l1
      else if l1 = 0 && l2 > 0 then
	dist + keyboard_max_distance * l2
      else
	let c1 = String.get s1 0 in
	let c2 = String.get s2 0 in
        let sub1 = String.sub s1 1 (l1-1) in 
        let sub2 = String.sub s2 1 (l2-1) in
        let char_score = keyboard_char_distance kmap c1 c2 in
        helper sub1 sub2 (dist + char_score)
    in helper s1 s2 0

  (*
   *  The probability of a match is:

      Pr = 1 - ( D / (L * M) )

      Where D is the distance between the two strings, L is the length of
      the longer string, and M is the maximum character distance.
   *)
  let keyboard_word_match kmap s1 s2 =
    let l1 = String.length s1 in
    let l2 = String.length s2 in
    let max_l = float_of_int (max l1 l2) in
    1. -. (float_of_int (keyboard_word_distance kmap s1 s2)) /. 
      (max_l *. (float_of_int keyboard_max_distance))


  let print_sample kmap =
    let s1 = "isntant" in
    let strings = ["instant"; "visitant"; "montant"; "intact"] in
    List.iter (fun str -> 
      Printf.printf "%s %s : %s\n" s1 str (string_of_float (keyboard_word_match kmap s1 str ))
    ) strings


  let unit_tests () =
    let kmap = build_map () in
    print_sample kmap
end
