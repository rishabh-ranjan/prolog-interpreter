(* TODO: error handling *)
{
    open Parser
}

let var = ['A'-'Z''_']['A'-'Z''a'-'z''0'-'9''_']*
let sym = ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*

rule scan = parse
| var as x { VAR x }
| sym as x { SYM x }
| '(' { LPAREN }
| ')' { RPAREN }
| ',' { COMMA }
| ":-" { IF }
| '.' { DOT }
    (* ignore whitespace *)
| ' '|'\t'|'\n'|'\r' { scan lexbuf }
