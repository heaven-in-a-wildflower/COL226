{
  open Lexing

  let line_number = ref 1

  type token =
    | V
    | Boolean
    | Integer of int
    | Float of float
    | Str
    | List
    | Open_list
    | Close_list
    | String of string
    | Length
    | Concatenate
    | Plus
    | Times
    | Assign
    | And
    | Or
    | Not
    | Eq
    | Gt
    | If
    | Then
    | Else
    | Identifier of string
    | Comment
    | Comma 
    | LP
    | RP
    | First
    | Second
    | Open_pair
    | Close_pair
    | End_instruction
    | End_program
  exception Eof

}
  let digit = ['0'-'9']
  let frac = '.' digit*
  let exp = ['e' 'E'] ['-' '+']? digit+
  let int = '-'?['1'-'9']['0'-'9']*
  let float = int* frac? exp?
  let string = '"'['a'-'z''A'-'Z''0'-'9']*'"'
  let comment = '#' [^'#']* '#'

rule token = parse
  | [' ' '\t']      { token lexbuf }               (* skip whitespace *)
  | ['\n']          { incr line_number; token lexbuf }  
  | "V"             { !line_number, V }
  | "Str"           { !line_number, Str} 
  | "list"          { !line_number, List}
  | "+"             { !line_number, Plus }
  | "*"             { !line_number, Times }
  | ":="            { !line_number, Assign }
  | "len"           { !line_number, Length} 
  | "and"           { !line_number, And }
  | "or"            { !line_number, Or }
  | "not"           { !line_number, Not }
  | "="             { !line_number, Eq }
  | ">"             { !line_number, Gt }
  | ","             { !line_number, Comma }
  | "("             { !line_number, LP }
  | ")"             { !line_number, RP }
  | "{"             { !line_number, Open_pair}
  | "}"             { !line_number, Close_pair}  
  | "["             { !line_number, Open_list}
  | "]"             { !line_number, Close_list}
  | comment         { !line_number, Comment}
  | "@"             { !line_number, Concatenate}
  | "%"             { !line_number, End_instruction}    
  | ";;"            { !line_number, End_program }
  | int             { !line_number, Integer(int_of_string (lexeme lexbuf)) }
  | float            { !line_number, Float(float_of_string (lexeme lexbuf)) }
  | string              { !line_number, String(lexeme lexbuf) }
  | "true"          { !line_number, Boolean }
  | "false"         { !line_number, Boolean }
  | "if"            { !line_number, If }
  | "then"          { !line_number, Then }
  | "else"          { !line_number, Else }
  | "fst"           { !line_number, First }
  | "snd"           { !line_number, Second }
  | ['0'-'9']['a'-'z''A'-'Z''0'-'9''_'''']* { raise (Failure "Invalid token") }
  | ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_'''']* { !line_number, Identifier(lexeme lexbuf) }
  | eof             { raise Eof }
and comment = parse
  | '#' {token lexbuf}
  | _ {comment lexbuf}
  | eof     {raise (Failure "Unterminated Comment")}
{
  let main() =
    try
      let filename = Sys.argv.(1) in
      let file_handle = open_in filename in 
      let lexbuf = Lexing.from_channel file_handle in 
      while true do 
        let line, result = token lexbuf in 
        Printf.printf "%d\t" line;
        match result with
        | Integer n -> Printf.printf "Integer\t%d\n" n
        | Float f -> Printf.printf "Float\t%f\n" f
        | Plus -> Printf.printf "Plus\n"
        | Str -> Printf.printf "Str\n"
        | List -> Printf.printf "List\n"
        | String s-> Printf.printf "String\t%s\n" s
        | Length -> Printf.printf "Length\n"
        | Concatenate -> Printf.printf "Concatenate\n"
        | Comment -> Printf.printf "Comment\n"
        | Times -> Printf.printf "Times\n"
        | Assign -> Printf.printf "Assign\n"
        | And -> Printf.printf "And\n"
        | Or -> Printf.printf "Or\n"
        | Not -> Printf.printf "Not\n"
        | Eq -> Printf.printf "Eq\n"
        | Gt -> Printf.printf "Gt\n"
        | If -> Printf.printf "If\n"
        | Then -> Printf.printf "Then\n"
        | Else -> Printf.printf "Else\n"
        | V -> Printf.printf "Variable\n"
        | Boolean -> Printf.printf "Boolean_constant\n"
        | Identifier x -> Printf.printf "Identifier\t%s\n" x
        | Comma -> Printf.printf "Comma\n"
        | LP -> Printf.printf "Left_Parenthesis\n"
        | RP -> Printf.printf "Right_Parenthesis\n"
        | Open_pair -> Printf.printf "Open_pair\n"
        | Close_pair -> Printf.printf "Close_pair\n"
        | Open_list -> Printf.printf "Open_list\n"
        | Close_list -> Printf.printf "Close_list\n"
        | First -> Printf.printf "First\n"
        | Second -> Printf.printf "Second\n"
        | End_instruction -> Printf.printf "End_instruction\n"
        | End_program -> Printf.printf "End_program\n"
      done
    with Eof -> exit 0

  let () = main()
}
