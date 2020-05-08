module Var = struct let compare = compare type t = int * string end
module VarMap = Map.Make(Var)

type term = Var of (int * string) | Node of (string * (term list))

type goal =
| Empty
| Term of term
| And of (goal * goal)
| Or of (goal * goal)

type clause = term * goal
type prog = clause list
type subst = term VarMap.t

type decision =
| G of goal
| K of (term * (clause list))

type node = {
    kb: clause list;
    sub: subst;
    scope: int;
    disjunct: node option;
    conjunct: goal option;
    decision: decision;
}
