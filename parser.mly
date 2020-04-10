/* vim: set ft=yacc: */
/* TODO: error handling */
%{
%}

%token <string> VAR SYM
%token LPAREN RPAREN COMMA SEMICOLON IF DOT
%token EOF

%start prog
%type <string Types.clause list> prog
%start clause
%type <string Types.clause> clause
%type <string Types.term list> term_list
%start term
%type <string Types.term> term
%start goal
%type <string Types.term list> goal

%%

prog: /* empty */ { [] }
    | clause DOT prog { $1::$3 }
;

clause: term { ($1, []) }
      | term IF term_list { ($1, $3) }
;

term_list: term { [$1] }
         | term COMMA term_list { $1::$3 }
;

term: VAR { Types.Var $1 }
    | SYM { Types.Node ($1, []) }
    | SYM LPAREN term_list RPAREN { Types.Node ($1, $3) }
;

goal: term_list DOT { $1 }
;
