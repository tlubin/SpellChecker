open Ranker
open Qwerty


module type SCORER =
sig
  (** type of the information used to score a result *)
  type score

  (* takes in potential matches and the typed word and returns its "score"
     along with other info: (freq, prob match, score). return back sorted *)
  val get_score : string list -> string -> (string*score) list

  (* calculates scores and returns whether or not all the scores are below a cutoff *)
  val all_low : string list -> string -> bool

end

module Score : SCORER with type score = int*float*float =
struct

  (* freq, prob match, score *)
  type score = int*float*float
  
  let ranker = Ranker.create Sys.argv.(4) (int_of_string Sys.argv.(3))
  let keyboard = QwertyKey.build_map()

  (* cutoff for being considered a "low" score *)
  let score_cutoff = 10.

  let get_score ms word =
    let get_info m = 
      let freq = Ranker.get_rank ranker m in
      let prob = QwertyKey.word_match keyboard m word in
      let score = (log10 (float_of_int (freq+1))) +. 10. *. prob in
      (m, (freq, prob, score))
    in 
    let unsorted = List.map get_info ms in
    List.sort (fun (_,(_,_,score)) (_,(_,_,score2)) -> compare score2 score) unsorted

  let all_low ms word =
    let scores = get_score ms word in
    not (List.exists (fun el -> let (_,(_,_,s)) = el in s >= score_cutoff) scores)

end
