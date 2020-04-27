/* vim: set ft=yacc: */
%{
    open Types
%}

%token <string> VAR ATOM
%token LPAREN RPAREN AND OR IF SEP
%token ERROR EOF

%right OR
%right AND

%start prog
%type <Types.prog> prog
%type <Types.clause> clause
%type <Types.term> term
%type <Types.goal> goal

%start query
%type <Types.goal> query

%%

prog: /* empty */ { [] }
    | clause SEP prog { $1::$3 }
    | error {
        let start_pos = Parsing.symbol_start_pos () in
        Printf.eprintf
            "\027[31mLine %d: syntax error\027[0m\n" (start_pos.pos_lnum);
        flush stderr; []
    }
;

clause: term { ($1, Empty) }
      | term IF goal { ($1, $3) }
;

goal: term { Term $1 }
    | LPAREN goal RPAREN { $2 }
    | goal AND goal { And ($1, $3) }
    | goal OR goal { Or ($1, $3) }
;

term: VAR { Var (0, $1) }
    | ATOM { Node ($1, []) }
    | ATOM LPAREN term_list RPAREN { Node ($1, $3) }
;

term_list: term { [$1] }
         | term AND term_list { $1::$3 }
;

query: goal SEP { $1 }
     | error {
         Printf.eprintf "\027[31msyntax error\027[0m\n";
         flush stderr; Empty
     }
;
