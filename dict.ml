module type DICT =
sig
  type dict_t

  (** create a dict_t given a filename string *)
  val create : string -> dict_t

  (** return the next valid entry in the dictionary at or past the input *)
  val next_entry: dict_t -> string -> string
end

module Dict : DICT =
struct
  type dict_t = string array

  let create filename =
    (* read in file from /usr/share/dict/web2 into an array *)
    let fp = open_in filename in
    let count = ref 0 in
    let calc_count =
      try
	while true do
	  (ignore (input_line fp);
	   incr count)
	done
      with End_of_file -> () in
    calc_count;
    close_in fp;
    let fp2 = open_in filename in
    let get_word i = String.lowercase (input_line fp2) in
    Array.init !count get_word
;;
    
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
    
    (*testing*)
  let d = create "/usr/share/dict/words";;
  next_entry d "aczz";;
  

end
