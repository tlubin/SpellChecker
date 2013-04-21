module type LEV =
sig
  val find_matches : string -> int -> (string -> string) -> string list
end

module Levenshtein : LEV =
struct
  class nfa :
    object
    method add_state: Type.state -> Type.transition -> Type.state -> unit
  end =
  object (self)

    val mutable states_dict = Map.Make(

    method add_state = (*implementation of add_state *)

    method private add_final_state = (* asjflaskjd *)

    (* lots of other things needed probably *)
  end

  let find_matches (word: string) (distance: int) (lookup_fun: string->string) =
    let word_nfa = build_nfa(word) in
    let word_dfa = to_dfa(word_nfa) in
    -- intersect with dictionary to return stuff within distance

  let build_nfa word =
    let nfa_word = new nfa in
    module OuterDict = Map.Make(Type.state) in
    module InnerDict = Map.Make(Type.transition) in
    let nfa = OuterDict.empty in
    (* some kind of recursive helper functions to build nfa with all the keys
       and values (innerdicts) *)

  let to_dfa nfa =
    (* convert nfa to dfa *)
end