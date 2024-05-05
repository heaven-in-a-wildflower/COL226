open Format

(* Define types of structures involved in the abstract syntax tree *)
type term =
  | Const of string
  | Var of string
  | Num of int
  | Func of string * term list
  | Tuple of term list

type atomic_formula = string * term list

type body = atomic_formula list

type clause =
  | Fact of atomic_formula
  | Rule of atomic_formula * body

type program = clause list

(* Define print functions for printing the AST *)
let rec print_indent n =
    if n <= 0 then ()
    else (
      Format.printf "\t";
      print_indent (n - 1)
    )
  
  let rec print_term_list  = function
    | [] -> ()
    | [x] -> print_term x
    | x :: xs ->
        print_term x;
        Format.printf ", ";
        print_term_list xs
  
  and print_term = function
    | Var s -> Format.printf " Variable : '%s' " s
    | Const s -> Format.printf " Constant : '%s' " s
    | Func (name,args) -> 
        Format.printf " Function : '%s' Arguments: (" name;
        print_term_list args;
        Format.printf ")"
    | Tuple(elems) ->
        Format.printf " Tuple : <";
        print_term_list elems;
        Format.printf ">"
    | Num d -> Format.printf " Numeral : '%d' " d 

  let rec print_atomic_formula (atom, terms) n =
    Format.printf "atomic_formula { \n" ;
    print_indent (n + 2);
    Format.printf "atom: '%s' \n" atom ; 
    print_indent (n + 2);
    Format.printf "term list : [ ";
    print_term_list terms;
    Format.printf " ] } "
  
  let rec print_body n = function
    | [] -> ()
    | af :: afs ->
        print_indent n;
        print_atomic_formula af (n-1);
        if afs <> [] then Format.printf ",\n";
        print_body n afs
  
  let rec print_clause n = function
    | Fact af ->
        print_indent n;
        Format.printf "Clause: Fact\n";
        print_indent n;
        Format.printf "{\n";
        print_indent (n + 1);
        Format.printf "head\n";
        print_indent (n + 1);
        Format.printf "{\n";
        print_indent (n + 2);
        print_atomic_formula af (n+1);
        Format.printf "\n";
        print_indent (n + 1);
        Format.printf "}\n";
        print_indent n;
        Format.printf "}\n"
    | Rule (af, body) ->
        print_indent n;
        Format.printf "Clause: Rule\n";
        print_indent n;
        Format.printf "{\n";
        print_indent (n + 1);
        Format.printf "head\n";
        print_indent (n + 1);
        Format.printf "{\n";
        print_indent (n + 2);
        print_atomic_formula af (n+1);
        Format.printf "\n";
        print_indent (n + 1);
        Format.printf "}\n";
        print_indent (n + 1);
        Format.printf "body\n";
        print_indent (n + 1);
        Format.printf "{\n";
        print_body (n + 2) body;
        Format.printf "\n";
        print_indent (n + 1);
        Format.printf "}\n";
        print_indent n;
        Format.printf "}\n"
  
    let print_ast program output_path =
        let output_channel = open_out output_path in
        set_formatter_out_channel output_channel;
        printf "Program\n{\n";
        List.iter (print_clause 1) program;
        printf "}\n";
        close_out output_channel
           
           
  