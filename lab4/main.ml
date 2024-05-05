open Lexing
open Lexer
open Parser
open Structure

let read_file file =
  let ic = open_in file in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let () =
  let input = read_file "input.txt" in
  let lexbuf = Lexing.from_string input in
  let ast = Parser.program Lexer.token lexbuf in
  Structure.print_ast ast "ast.txt";
  