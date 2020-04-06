module StringMap = Map.Make(String)

type 'a term = Var of 'a | Node of string * ('a term list)
type 'a clause = ('a term) * ('a term list)
