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

type goal_state =
| G_end
| G_new of int goal
| G_term of term_state
| G_and of goal_state * goal_state
| G_or of goal_state * goal_state

and term_state =
| T_end
| T_new of int term
| T_term of int term * int prog
| T_goal of goal_state * int term * int prog

