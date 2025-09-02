%{
open Ast
%}

%token LET IN CASE OF END SAMPLE TRUE FALSE
%token <string> IDENT
%token LPAREN RPAREN COMMA EQ ARROW BAR
%token EOF

%start <Ast.term> program

%%

program:
  | term EOF { $1 }

term:
  | LET IDENT EQ term IN term { Let($2, $4, $6) }
  | LET LPAREN IDENT COMMA IDENT RPAREN EQ value IN term { Let2($3, $5, $8, $10) }
  | CASE value OF branches END { Case($2, $4) }
  | CASE value OF branches error { raise (Failure "expected 'end' to close case") }
  | SAMPLE { Sample }
  | value { Val $1 }

branches:
  | branch { [$1] }
  | branch BAR branches { $1 :: $3 }

branch:
  | value ARROW term { ($1, $3) }

value:
  | TRUE { True }
  | FALSE { False }
  | IDENT { Var $1 }
  | LPAREN value COMMA value RPAREN { Pair($2, $4) }
