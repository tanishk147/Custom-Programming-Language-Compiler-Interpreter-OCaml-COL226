(* main.ml *)
open Ast
open Parsing
open Parser
open Lexer


(* Optional: Add a function to convert tokens to strings for debugging *)
let token_to_string t = match t with 
  | INPUT -> "INPUT"
  | PRINT -> "PRINT"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | FILENAME s -> "FILENAME(" ^ s ^ ")"
  | BOOL_TYPE -> "BOOL_TYPE"
  | INT_TYPE -> "INT_TYPE"
  | FLOAT_TYPE -> "FLOAT_TYPE"
  | VECTOR_TYPE -> "VECTOR_TYPE"
  | MATRIX_TYPE -> "MATRIX_TYPE"
  | IDENTIFIER s -> "IDENTIFIER(" ^ s ^ ")"
  | INT_CONST i -> "INT_CONST(" ^ string_of_int i ^ ")"
  | FLOAT_CONST f -> "FLOAT_CONST(" ^ string_of_float f ^ ")"
  | BOOL_CONST b -> "BOOL_CONST(" ^ string_of_bool b ^ ")"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | SEMICOLON -> "SEMICOLON"
  | COMMA -> "COMMA"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | ASSIGN -> "ASSIGN"
  | MOD -> "MOD"
  | DOT -> "DOT"
  | TRANSPOSE -> "TRANSPOSE"
  | EQ -> "EQ"
  | LT -> "LT"
  | GT -> "GT"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | TO -> "TO"
  | DO -> "DO"
  | END -> "END"
  | NEQ -> "NEQ"
  | DETERMINANT -> "DETERMINANT"
  | EOF -> "EOF"
  | STRING s -> "STRING(" ^ s ^ ")"
  | READ_MATRIX -> "READ_MATRIX"
  | INVERSE -> "INVERSE"
  | SQRT -> "SQRT"
;;

  


let print_token_stream lexbuf =
  let rec print_tokens () =
    let token = Lexer.tokenize lexbuf in
    (match token with
      | EOF -> 
          print_endline "EOF";
          flush stdout
      | _ -> (
        print_endline (token_to_string token);
        flush stdout;
        print_tokens ()
      )
    )
  in
  print_tokens ()

let () =
  let lexbuf = 
    if Array.length Sys.argv > 1 then
      let file = open_in Sys.argv.(1) in
      Lexing.from_channel file
    else
      Lexing.from_channel stdin
  in
  
  try
    (* Print the tokens for debugging *)
    let debug_lexbuf = 
      if Array.length Sys.argv > 1 then
        let file = open_in Sys.argv.(1) in
        Lexing.from_channel file
      else
        Lexing.from_channel stdin
    in
    print_endline "--- Token Stream ---";
    flush stdout;
    print_token_stream debug_lexbuf;
    print_endline "--- End Token Stream ---";
    flush stdout;
    
    (* Parse with original lexbuf *)
    let commands = program Lexer.tokenize lexbuf in
    print_endline "Parsing succeeded! AST:";
    flush stdout;
    print_endline (Ast.string_of_command commands);
    flush stdout;
    
    (* Execute the parsed commands *)
    let env = Interpreter.create_env () in
    print_endline "Executing program...";
    flush stdout;
    Interpreter.eval_command env commands;
    print_endline "Execution completed successfully.";
    flush stdout
    
  with
  | Lexer.Lexing_error msg -> 
      Printf.eprintf "Lexer error: %s\n" msg;
      flush stderr
  | Parsing.Parse_error ->
      Printf.eprintf "Syntax error at line %d, character %d\n" 
        (Lexing.lexeme_start_p lexbuf).pos_lnum
        ((Lexing.lexeme_start_p lexbuf).pos_cnum - (Lexing.lexeme_start_p lexbuf).pos_bol);
      Printf.eprintf "Near token: '%s'\n" (Lexing.lexeme lexbuf);
      flush stderr
  | Type_checker.TypeError msg ->
      Printf.eprintf "Type error: %s\n" msg;
      flush stderr
  | Interpreter.RuntimeError msg ->
      Printf.eprintf "Runtime error: %s\n" msg;
      flush stderr