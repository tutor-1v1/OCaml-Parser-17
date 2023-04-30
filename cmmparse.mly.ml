https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
/* Use the expression datatype defined in expressions.ml: */
%{
  open Common

(* You may want to add extra code here *)

%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT /* CHAR */
%token <Common.basic_type> TYPESPEC
%token NEG PLUS MINUS TIMES DIV  MOD CARAT ARRAYDEC
LT GT LEQ GEQ EQUALS NEQ NOT LOGICALAND LOGICALOR
ASSIGN SEMI COMMA  LPAREN RPAREN LBRAC RBRAC LBRACE RBRACE 
TRUE FALSE IF ELSE WHILE RETURN PRINT VOID EOF

/* Define the "goal" nonterminal of the grammar: */
%start main program decl statement varDecl exp
%type <Common.untyped_program> main program
%type <Common.untyped_decl> decl
%type <Common.untyped_statement> statement
%type <Common.varDecl> varDecl
%type <Common.untyped_exp> exp
%%

/*
Enough has been put here to let this compile.  You will want to add more rules, 
including adding more stratification, and possibly move some things around and 
reorganize it.
*/

main:
  | program EOF      		        { $1 }


program:
  | decl_list exp 	                { ($1, $2) }

decl_list:
  | decl			        { [$1] }

decl:
  | varDecl                             { VarDecl $1 }

varDecl:
  | TYPESPEC varDeclID_list SEMI        { ($1, $2) }

varDeclID_list:
  | varDeclID                           { [$1] }

varDeclID:
  | IDENT                               { VarId($1) }

statement:
  | exp SEMI                            { ExpStatement($1) }

exp:
  | constant_exp                        { ConstExp $1 }

constant_exp:
  |  INT                                { IntConst $1 }
