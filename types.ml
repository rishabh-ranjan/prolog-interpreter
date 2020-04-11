module Int = struct let compare = compare type t = int end
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

type 'a term = Var of 'a | Node of string * ('a term list)

type 'a goal =
| Empty
| Term of 'a term
| And of 'a goal * 'a goal
| Or of 'a goal * 'a goal

type 'a clause = 'a term * 'a goal

type 'a prog = 'a clause list

type subst = int term IntMap.t

type state =
| End
| New of int goal
| And_goal of state * state * int goal
| Or_goal of state * state
| Head of int term * int prog
| Body of int term * int prog * int list * subst * state

