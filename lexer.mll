{
    open Parser

    (* To keep track of the line numbers for better error msgs *)
    let incr_linenum lexbuf =
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with
          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        }
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
| ',' { AND }
| ';' { OR }
| ":-" { IF }
| '.' { SEP }
| var as x { VAR x }
| atom as x { ATOM x }
    (* ignore whitespace *)
| ' '|'\t'|'\r' { scan lexbuf }
    (* newline *)
| '\n' { incr_linenum lexbuf; scan lexbuf }
    (* exception case - let the parser handle the error *)
| _ { ERROR }
| eof { EOF }
