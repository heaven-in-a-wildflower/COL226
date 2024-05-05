{
  open Parser;;
  exception InvalidToken of char ;;
}

let alpha_num = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let var = ['A'-'Z'](alpha_num*)
let cons = ['a'-'z'](alpha_num*) | ("\"" [^ '\"']+ "\"")
let sp = [' ' '\t' '\n']+

rule read = parse
    eof                   {EOF}
  | sp                    {read lexbuf}
  | var as v              {VAR(v)}
  | cons as c             {CONS(c)} 
  | '('                   {LP}
  | ')'                   {RP}
  | ','                   {COMMA}
  | '!'                   {CUT}
  | '.'                   {ENDL}
  | ":-"                  {FOLLOWS}
  | '%'                   {single_line_comment lexbuf}
  | _ as s                {raise (InvalidToken s)}

and single_line_comment = parse
    eof                   {EOF}
  | '\n'                  {read lexbuf}
  |   _                   {single_line_comment lexbuf}
