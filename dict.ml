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
    let rec count (n: int) (temp: in_channel) : int =
      try
	ignore (input_line temp);
	count (n+1) temp
      with End_of_file -> n in
    (* make array of size (count 0 fp) *)
    (* move fp back to the start and then read in the words into the array and return the array *)

  let next_entry dict word =
    (* recursively find the next word, inclusive *)
    failwith "implement"

end
