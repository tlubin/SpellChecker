
module type RANKER =
sig
  type t
  val create: string -> int -> t
  val get_rank: t -> string -> int option
end

module Ranker : RANKER =
struct

  module D = Map.Make(String)

  type t = int D.t

  (* split a string around the first instance of a char identifier *)
  let split str i =
    let index = String.index str i in
    (String.sub str 0 index, String.sub str (index+1) ((String.length str)-index-1))

  let create filename count =
    let fp = open_in filename in
    let rec build_map dict n =
      if n <= 0 then dict 
      else
	let l = input_line fp in
	let str, freq = split l ' ' in
	let dict' = D.add (String.lowercase str) (int_of_string freq) dict in 
	build_map dict' (n-1) in
    build_map (D.empty) count

  let get_rank ranker str = 
    try
      Some (D.find str ranker)
    with Not_found -> None

end
