open Lexer;;
open Parser;;
open Interpreter;;

let fstream = open_in Sys.argv.(1);;
let p = Parser.program Lexer.read (Lexing.from_channel fstream);;

print_string "Database loaded\n";;

while(true) do
  print_string "?- ";
  let query = read_line() in
  try
    let g = Parser.goal Lexer.read (Lexing.from_string query) in
    match (my_swipl g p) with
        (_,true) -> print_string "true.\n"
      | (_,false) -> print_string "false.\n"
  with e -> Printf.printf "%s\n" (Printexc.to_string e)
done

