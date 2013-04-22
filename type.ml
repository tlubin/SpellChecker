type state = int*int

module StateSet = Set.Make(
struct
  type t = int*int
  let compare a b = compare a b
end)
