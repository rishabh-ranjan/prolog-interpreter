module IntMap = Map.Make(struct let compare = compare type t = int end)
module StringMap = Map.Make(String)

type 'a term = Var of 'a | Node of 'a * ('a term list)
type 'a clause = ('a term) * ('a term list)
