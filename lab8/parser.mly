%{
    open Interpreter;;
%}

%token <string> VAR CONS
%token LP RP COMMA ENDL CUT FOLLOWS EOF

%left COMMA
%nonassoc ENDL

%start program goal
%type <Interpreter.program> program
%type <Interpreter.goal> goal
%%

program:
  | EOF                                 {Prog([])}
  | clause_list EOF                     {Prog($1)}
;

clause_list:
  | clause                              {[$1]}
  | clause clause_list                  {($1)::$2}
;

clause:
  | atom ENDL                           {Fact(Head($1))}
  | atom FOLLOWS atom_list ENDL         {Rule(Head($1), Body($3))}
;

goal:
  | atom_list ENDL                      {Goal($1)}
;

atom_list:
  | atom                                {[$1]}
  | atom COMMA atom_list                {($1)::$3}
;

atom:
  | CONS                                {Atom($1, [])}
  | CONS LP term_list RP                {Atom($1, $3)}
  | CUT                                 {Atom("&cut", [])}
;

term_list:
  | term                                {[$1]}
  | term COMMA term_list                {($1)::$3}
;

term:
  | LP term RP                          {$2}
  | VAR                                 {Var($1)}
  | CONS                                {Tree_node($1, [])}
  | CONS LP term_list RP                {Tree_node($1, $3)}
;
