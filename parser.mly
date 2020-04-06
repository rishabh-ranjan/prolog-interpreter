/* vim: set ft=yacc: */
/* TODO: error handling */
%{
%}

%token <string> VAR SYM
%token LPAREN RPAREN COMMA IF DOT
%token EOF /* TODO: ?? */

%start prog
%type <string Types.clause list> prog
%type <string Types.clause> clause
%type <string Types.term> term
%start goal
%type <string Types.term list> goal

%%

prog: /* empty */ { [] }
    | clause DOT prog { $1::$3 }
;

clause: term { ($1, []) }
      | term IF goal { ($1, $3) }
;

goal: term { [$1] }
    | term COMMA goal { $1::$3 }
;

term: VAR { Types.Var $1 }
    | SYM { Types.Node ($1, []) }
    | SYM LPAREN goal RPAREN { Types.Node ($1, $3) }
;
