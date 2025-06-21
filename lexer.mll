(* lexer.mll *)
{
  exception Lexing_error of string
  open Parser
}

(* Regular expression definitions *)
let digit = ['0'-'9']
let integer = digit+
let float = digit+ '.' digit+
let alpha = ['a'-'z' 'A'-'Z']
let identifier = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t' '\n']
let filename = '"' [^ '"']* '"'

rule tokenize = parse
  (* Whitespace and comments *)
  | whitespace    { tokenize lexbuf }
  | "//"[^'\n']*  { tokenize lexbuf }  (* Single-line comments *)
  | "/*"          { comment lexbuf }   (* Multi-line comments *)
  | '"' [^'"']* '"' as s { STRING(String.sub s 1 (String.length s - 2)) }
  
  | filename      { FILENAME(Lexing.lexeme lexbuf) }


  (* IO Commands *)
  | "Print"       { PRINT }
  | "Input"       { INPUT }

  (* Types *)
  | ['M''m']['A''a']['T''t']['R''r']['I''i']['X''x'] { MATRIX_TYPE }
  | "bool"        { BOOL_TYPE }
  | "int"         { INT_TYPE }
  | "float"       { FLOAT_TYPE }
  | "vector"      { VECTOR_TYPE }
  | "inv"    { INVERSE }
  | "sqrt"        { SQRT }

  (* Constants *)
  | "true"        { BOOL_CONST(true) }
  | "false"       { BOOL_CONST(false) }
  | float as f    { FLOAT_CONST(float_of_string f) }
  | integer as i  { INT_CONST(int_of_string i) }

  (* Delimiters *)
  | '('          { LPAREN }
  | ')'          { RPAREN }
  | '['          { LBRACKET }
  | ']'          { RBRACKET }
  | '{'          { LBRACE }
  | '}'          { RBRACE }
  | ';'          { SEMICOLON }  (* Ensure SEMICOLON is produced for ; *)
  | ','          { COMMA }

  (* Operators *)
  | '+'          { PLUS }
  | '-'          { MINUS }
  | '*'          { TIMES }
  | '/'          { DIV }
  | ":="         { ASSIGN }  
  | '%'          { MOD }
  | '.'          { DOT }
  | '\''         { TRANSPOSE }

  (* Comparison *)
  | "="          { EQ }
  | "<"          { LT }
  | ">"          { GT }
  | "<="         { LEQ }
  | ">="         { GEQ }
  | "<>"         { NEQ }

  (* Keywords *)

  (* Boolean operators *)
  | "and"        { AND }
  | "or"         { OR }
  | "not"        { NOT }
  | "determinant" { DETERMINANT }

  (* Control structures *)
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }
  | "while"      { WHILE }
  | "for"        { FOR }
  | "to"         { TO }
  | "do"         { DO }
  | "read_matrix" { READ_MATRIX }

  
  (* Variables *)
  | identifier as id { IDENTIFIER(id) }
  | eof          { EOF }
  | _ as c       { raise (Lexing_error ("Unexpected character: " ^ String.make 1 c)) }

and comment = parse
  | "*/"         { tokenize lexbuf }  (* End comment, return to normal lexing *)
  | _            { comment lexbuf }
  | eof          { raise (Lexing_error "Unterminated comment") }
