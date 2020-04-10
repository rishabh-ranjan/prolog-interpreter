module Int = struct let compare = compare type t = int end
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

type 'a term = Var of 'a | Node of string * ('a term list)
type 'a goal =
    | Term of term
    | And of ('a goal) * ('a goal)
    | Or of ('a goal) * ('a goal)
type 'a clause = ('a term) * ('a goal)
type 'a prog = 'a clause list
