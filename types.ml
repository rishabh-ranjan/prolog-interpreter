module Var = struct let compare = compare type t = int * string end
module VarMap = Map.Make(Var)

type term = Var of (int * string) | Node of string * (term list)

type goal =
| Empty
| Term of term
| And of goal * goal
| Or of goal * goal

type clause = term * goal

type prog = clause list

type subst = term VarMap.t

type state =
| End
| New of goal
| And_goal of state * state * goal
| Or_goal of state * goal
| Head of term * prog
| Body of int * term * prog * subst * state

