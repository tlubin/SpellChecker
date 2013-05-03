module type DICT =
sig
  type t

  (** create a dict_t given a filename string *)
  val create : string -> int -> t

  (** functions to get letters from alphabet *)
  val last_letter : char
  val first_letter : char
  val next_letter : char -> char

  (** return the next valid entry in the dictionary at or past the input *)
  val next_entry: t -> string -> string

(*  val unit_tests : unit -> unit *)
end

module Dict : DICT =
struct
  type t = string array
      
  let last_letter = 'z'
  let first_letter = 'a'

  (* do not call on the last_letter of the alphabet *)
  let next_letter l =  Char.chr ((Char.code l) + 1)

  let create filename count =
    let fp = open_in filename in
    let get_word i = String.lowercase (input_line fp) in
    let a = Array.init count get_word in
    close_in fp; a
    
    (* make array of size (count 0 fp) *)
    (* move fp back to the start and then read in the words into
       the array and return the array *)

  let next_entry dict word =
    (* recursively find the next word, inclusive *)
    let rec helper (s:int) (e:int) : string =
      if e=s then
	if (e != (Array.length dict)-1) then
	  (let here = Array.get dict s in
	   let next = Array.get dict (s+1) in
	   if ((String.compare word here) < 0) then here
	   else if ((String.compare word here) > 0) then next
	   else (* String.compare word here = 0 *) word)
	else "" (* last string in dict *)
      else 
      (let mid = (s+e)/2 in
      let compared = String.compare word (Array.get dict mid) in
      if compared = 0 then word
      else if compared < 0 then helper s mid
      else (* if compared > 0 then *) helper (mid+1) e)
    in
    helper 0 ((Array.length dict)-1)
    
(*  (*testing*)
  let unit_tests () =
    let d = create "/usr/share/dict/web2" in
    assert(next_entry d "abstracta" = "abstracted");
    assert(next_entry d "acinara" = "acinarious");
    assert(next_entry d "flooded" = "flooded");
    ()
*)
end
