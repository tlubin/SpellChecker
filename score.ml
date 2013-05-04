open Ranker
open Qwerty

module type SCORER =
sig

  (* takes in potential matches and the typed word and returns its "score"
     and return back sorted *)
  val get_score : string list -> string -> (string*float) list

  (* return extra info: (freq, prob match, score) *)
  val get_score_extra : string list -> string -> (string*int*float*float) list

  (* calculates scores and returns whether or not all the scores are below a cutoff *)
  val all_low : string list -> string -> bool

end

module Score : SCORER =
struct
  
  let ranker = Ranker.create Sys.argv.(4) (int_of_string Sys.argv.(3))
  let keyboard = QwertyKey.build_map()

  let score_cutoff = 10.

  let get_score_extra ms word =
    let get_info m = 
      let freq = Ranker.get_rank ranker m in
      let prob = QwertyKey.word_match keyboard m word in
      let score = (log10 (float_of_int (freq+1))) +. 10. *. prob in
      (m, freq, prob, score)
    in 
    let unsorted = List.map get_info ms in
    List.sort (fun (_,_,_,score) (_,_,_,score2) -> compare score2 score) unsorted

  let get_score ms word = List.map 
    (fun tuple -> let m,_,_,score = tuple in (m,score))
    (get_score_extra ms word)

  let all_low ms word =
    let scores = get_score ms word in
    not (List.exists (fun (_,score) -> score >= score_cutoff) scores)

end
