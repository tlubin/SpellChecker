type tran = Correct of char | Other

module type DFA =
sig
  type dfa_t
  
  (** return the next valid string satisfying the dfa *)
  val next_valid_string: string -> string

    (* stuff needed to build the dfa. i.e. add_transition,... *)
end

module Dfa : DFA =
  failwith "implement me"
