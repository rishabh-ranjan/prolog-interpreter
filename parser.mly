/* vim: set ft=yacc: */
%{
%}

%token <string> VAR ATOM
%token LPAREN RPAREN AND OR IF SEP
%token ERROR EOF

%right OR
%right AND

%start prog
%type <string Types.prog> prog
%type <string Types.clause> clause
%type <string Types.term> term
%type <string Types.goal> goal

%start query
%type <string Types.goal> query

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

clause: term { ($1, Types.Empty) }
      | term IF goal { ($1, $3) }
;

goal: term { Types.Term $1 }
    | LPAREN goal RPAREN { $2 }
    | goal AND goal { Types.And ($1, $3) }
    | goal OR goal { Types.Or ($1, $3) }
;

term: VAR { Types.Var $1 }
    | ATOM { Types.Node ($1, []) }
    | ATOM LPAREN term_list RPAREN { Types.Node ($1, $3) }
;

term_list: term { [$1] }
         | term AND term_list { $1::$3 }
;

query: goal SEP { $1 }
     | error {
         Printf.eprintf "\027[31msyntax error\027[0m\n";
         flush stderr; Types.Empty
     }
;
