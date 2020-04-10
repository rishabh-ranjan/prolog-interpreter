(* TODO: error handling *)
{
    open Parser
}

let var = ['A'-'Z''_']['A'-'Z''a'-'z''0'-'9''_']*
let atom =
    (['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*)
    | ('''[^''']*''')
    | "[]"
    | (['+''-''*''/''<''>''='':''.''&''_''~']+)

rule scan = parse
| '(' { LPAREN }
| ')' { RPAREN }
| ',' { COMMA }
| ';' { SEMICOLON }
| ":-" { IF }
| '.' { DOT }
| var as x { VAR x }
| atom as x { SYM x }
    (* ignore whitespace *)
| ' '|'\t'|'\n'|'\r' { scan lexbuf }
| eof { EOF }
