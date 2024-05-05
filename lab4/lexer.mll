{
 open Parser
 exception InvalidToken of char 
 (* type token = 
    |COMMA
    |LP
    |RP
    |PERIOD
    |FOLLOWS
    |ATOM of string     (*Represents name of predicate*)
    |VARIABLE of string
    |EOF *)
}

let alpha_num = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let var = ['A'-'Z'](alpha_num*)
let atom = ['a'-'z'](alpha_num*) | ("\"" [^ '\"']+ "\"")
let sp = [' ' '\t' '\n']+
let comment = '#' [^'#']* '#'

rule token = parse
  | sp                    { token lexbuf }
  | var as v              { VARIABLE(v) }
  | atom as c             { ATOM(c) }
  | '('                   { LP }
  | ')'                   { RP }
  | '<'                     {LB}
  | '>'                     {RB}
  | ','                   { COMMA }
  | ';'                   {SC}
  | ['0'-'9']+ as lxm { NUM(int_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '.'                   { PERIOD }
  | ":-"                  { FOLLOWS }
  | '#'                   { comment lexbuf }  (* Skip comments *)
  | _ as s                { raise (InvalidToken s) }
  | eof                   { EOF }
and comment = parse
  | '\n'                  { token lexbuf }     (* Newline terminates the comment *)
  | _                     { comment lexbuf }   (* Skip characters within the comment *)
(* {
 let main() = 
  try
    let filename = Sys.argv.(1) in
    let file_handle = open_in filename in 
    let lexbuf = Lexing.from_channel file_handle in 
    while true do 
      let token_result = token lexbuf in 
      match token_result with
      | VARIABLE(v) -> Printf.printf "VARIABLE\t%s\n" v
      | ATOM(c) -> Printf.printf "ATOM\t%s\n" c 
      | FOLLOWS -> Printf.printf "FOLLOWS\n"
      | PERIOD -> Printf.printf "PERIOD\n"
      | COMMA -> Printf.printf "COMMA\n"
      | LP -> Printf.printf "LP\n"
      | RP -> Printf.printf "RP\n"
      | EOF -> Printf.printf "EOF\n"; exit 0
    done
  with
  | Invalid_argument _ -> Printf.printf "Usage: %s filename\n" Sys.argv.(0)
  | Sys_error msg -> Printf.printf "Error: %s\n" msg

let () = main()

} *)
