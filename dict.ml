module type DICT =
sig
  type dict_t

  (** create a dict_t given a filename string *)
  val create : string -> dict_t

  (** return the next valid entry in the dictionary at or past the input *)
  val next_entry: string -> string
end

module Dict : DICT =
  failwith "implement me"
