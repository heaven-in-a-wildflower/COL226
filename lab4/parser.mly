%{
  open Structure
%}

(*Define tokens*)
%token COMMA
%token LP
%token RP
%token LB
%token RB
%token SC
%token PERIOD
%token FOLLOWS
%token PLUS MINUS TIMES DIV
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%token <int> NUM
%token <string> VARIABLE
%token <string> ATOM 
%token EOF

(*Note that the parser defines the terms in the grammar,however the type of the term is inferred from structure.ml*)
%start program
%type <Structure.program> program
%type <Structure.clause> clause
%type <Structure.clause list> clause_list
%type <Structure.atomic_formula> atomic_formula
%type <Structure.term> term
%type <Structure.term list> term_list
%type <Structure.body> body

%%

(*Define structure of a Prolog-like program*)
(*Ensure that grammar is right-recursive*)
program: 
 | clause_list EOF { $1 }

clause_list: 
 | clause { [$1] }
 | clause clause_list { $1 :: $2 } 

clause:
 | head PERIOD { Fact $1 }
 | head FOLLOWS body PERIOD { Rule ($1, $3) }

head:
 | atomic_formula { $1 }

body:
 | atomic_formula { [$1] }
 | atomic_formula COMMA body { $1 :: $3 } (*Body is a list of atomic formulas*)

atomic_formula:
 | ATOM LP term_list RP { ($1, $3) } (*Tuple of atom and term list*)

term_list:
 | term { [$1] }
 | term COMMA term_list { $1 :: $3 } 

term:
 | VARIABLE { Var $1 }
 | ATOM { Const $1 }
 | tuple {$1}
 | func {$1}
 | num { Num $1 }

func:
 | ATOM LP term_list RP { Func($1, $3) }

tuple:
 | LB term_list RB { Tuple($2)}

num:
  | NUM                   { $1 }
  | LP num RP             { $2 }
  | num PLUS num          { $1 + $3 }
  | num MINUS num         { $1 - $3 }
  | num TIMES num         { $1 * $3 }
  | num DIV num           { $1 / $3 }
  | MINUS num %prec UMINUS { - $2 }
;
%%
