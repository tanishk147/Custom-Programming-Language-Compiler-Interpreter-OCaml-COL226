%{
    open Ast
    open Type_checker
%}


%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <bool> BOOL_CONST
%token <string> IDENTIFIER
%token <string> FILENAME
%token <string> STRING
%token INPUT PRINT IF THEN ELSE FOR WHILE DO END TO
%token BOOL_TYPE INT_TYPE FLOAT_TYPE VECTOR_TYPE MATRIX_TYPE
%token ASSIGN EQ NEQ LEQ GEQ AND OR NOT
%token PLUS MINUS TIMES DIV MOD LT GT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA SEMICOLON DOT TRANSPOSE DETERMINANT EOF
%token READ_MATRIX
%token INVERSE
%token SQRT


/* Precedence declarations - lower precedence comes first */
%right ASSIGN
%nonassoc THEN  /* This and the next line handle the dangling else problem */
%nonassoc ELSE
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT
%left TRANSPOSE DETERMINANT INVERSE SQRT
%left DOT
%nonassoc LBRACKET
%nonassoc UMINUS  

%start program
%type <Ast.command> program


%%

program:
  | command_list EOF { Seq($1) }

command_list:
  | command command_list { $1 :: $2 }
  | command { [$1] }

command:
    | block_command { $1 }
    | simple_command { $1 }

block_command:
    | LBRACE command_list RBRACE { Seq($2) }
    | IF expr THEN command ELSE command { 
        if type_expr $2 <> TBool then 
            raise (TypeError "Condition must be boolean");
        IfThenElse($2, $4, $6) 
    }
    | IF expr THEN command { 
        if type_expr $2 <> TBool then 
            raise (TypeError "Condition must be boolean");
        IfThenElse($2, $4, Seq([])) 
    }
    | FOR IDENTIFIER ASSIGN expr TO expr DO command { 
        if type_expr $4 <> TInt || type_expr $6 <> TInt then 
            raise (TypeError "Loop bounds must be integers");
        ForLoop($2, $4, $6, $8) 
    }   
    | WHILE expr DO command { 
        if type_expr $2 <> TBool then 
            raise (TypeError "While condition must be boolean");
        WhileLoop($2, $4) 
    }

simple_command:
    | INPUT LPAREN RPAREN SEMICOLON { Input None }
    | INPUT LPAREN expr RPAREN SEMICOLON { Input(Some $3) }
    | PRINT LPAREN expr RPAREN SEMICOLON { Print($3) }
    | variable_declaration SEMICOLON { $1 }
    | IDENTIFIER ASSIGN expr SEMICOLON { 
        let var_type = 
            try Hashtbl.find symbol_table $1 
            with Not_found -> raise (TypeError ("Undefined variable: " ^ $1)) in
        let expr_type = type_expr $3 in
        if not (types_compatible var_type expr_type) then
            raise (TypeError (Printf.sprintf "Type mismatch in assignment to %s: expected %s, got %s" 
                   $1 (type_to_string var_type) (type_to_string expr_type)));
        Assign($1, $3) 
    }

    | expr ASSIGN expr SEMICOLON {
        match $1 with
        | IndexAccess(_, _) | MatrixAccess(_, _, _) ->
            let access_type = type_expr $1 in
            let expr_type = type_expr $3 in
            if not (types_compatible access_type expr_type) then
                raise (TypeError "Type mismatch in indexed assignment");
            IndexAssign($1, $3)
        | _ -> raise (TypeError "Left side of assignment must be a variable or index access")
    }
    

variable_declaration:
    | typed_ident ASSIGN expr { 
        let (id, decl_type) = $1 in
        let expr_type = type_expr $3 in
        if not (types_compatible decl_type expr_type) then
            raise (TypeError (Printf.sprintf "Type mismatch in declaration of %s: expected %s, got %s" 
                   id (type_to_string decl_type) (type_to_string expr_type)));
        Hashtbl.add symbol_table id decl_type;
        Declare(id, decl_type, Some $3)
    }
    | typed_ident{ 
        let (id, decl_type) = $1 in
        Hashtbl.add symbol_table id decl_type;
        Declare(id, decl_type, None) 
    }

expr:
    | simple_expr { $1 }
    | indexed_expr { $1 }
    | unary_expr { $1 }
    | binary_expr { $1 }
    | postfix_expr { $1 }
    | LPAREN expr RPAREN { $2 }
    | READ_MATRIX LPAREN STRING COMMA INT_CONST COMMA INT_CONST RPAREN 
    { ReadMatrix($3, $5, $7) }

postfix_expr:
    |expr TRANSPOSE { 
        match type_expr $1 with
        | TMatrix(m, n) -> Transpose($1)
        | TVector(n) -> Transpose($1)
        | _ -> raise (TypeError "Transpose can only be applied to matrices or vectors")
    }

unary_expr:
    | TRANSPOSE expr { 
        match type_expr $2 with
        | TMatrix(m, n) -> Transpose($2)
        | TVector(n) -> Transpose($2)
        | _ -> raise (TypeError "Transpose can only be applied to matrices or vectors")
    }
    | DETERMINANT expr { 
        match type_expr $2 with
        | TMatrix(m, n) when m = n -> Determinant($2)
        | _ -> raise (TypeError "Determinant can only be applied to square matrices")
    }
    | NOT expr {
        if type_expr $2 <> TBool then
            raise (TypeError "NOT operator requires boolean operand");
        UnOp(Not, $2)
    }
    | MINUS expr %prec UMINUS {
        match type_expr $2 with
        | TInt -> UnOp(Neg, $2)
        | TFloat -> UnOp(Neg, $2)
        | TVector _ -> UnOp(Neg, $2)
        | TMatrix _ -> UnOp(Neg, $2)
        | _ -> raise (TypeError "Unary minus can only be applied to numeric types")
    }
    | INVERSE expr {
        match type_expr $2 with
        | TMatrix(m, n) when m = n -> Inverse($2)
        | _ -> raise (TypeError "Inverse can only be applied to square matrices")
    }
    | SQRT expr { 
        match type_expr $2 with
        | TInt | TFloat -> UnOp(Sqrt, $2)
        | _ -> raise (TypeError "Square root requires numeric operand")
    }

binary_expr:
    | expr PLUS expr { typed_binop $1 Add $3 }
    | expr MINUS expr { typed_binop $1 Sub $3 }
    | expr TIMES expr { typed_binop $1 Mul $3 }
    | expr DIV expr { typed_binop $1 Div $3 }
    | expr MOD expr { typed_binop $1 Mod $3 }
    | expr EQ expr { comparison_op $1 Eq $3 }
    | expr NEQ expr { comparison_op $1 Neq $3 }
    | expr LT expr { comparison_op $1 Lt $3 }
    | expr GT expr { comparison_op $1 Gt $3 }
    | expr LEQ expr { comparison_op $1 Leq $3 }
    | expr GEQ expr { comparison_op $1 Geq $3 }
    | expr AND expr { logical_op $1 And $3 }
    | expr OR expr { logical_op $1 Or $3 }
    | expr DOT expr { 
        match type_expr $1, type_expr $3 with
        | TVector n, TVector m when n = m -> DotProduct($1, $3)
        | _ -> raise (TypeError "Dot product requires vectors of the same size")
    }

simple_expr:
    | BOOL_CONST { BoolLit($1) }
    | INT_CONST { IntLit($1) }
    | FLOAT_CONST { FloatLit($1) }
    | STRING { StringLit($1) }
    | IDENTIFIER { 
        try 
            let _ = Hashtbl.find symbol_table $1 in 
            Var($1) 
        with Not_found -> 
            raise (TypeError ("Undefined variable: " ^ $1)) 
    }
    | vector_literal { $1 }
    | matrix_literal { $1 }

indexed_expr:
    | expr LBRACKET expr RBRACKET %prec LBRACKET { 
        match type_expr $1, type_expr $3 with
        | TVector _, TInt -> IndexAccess($1, $3)
        | TMatrix _, TInt -> IndexAccess($1, $3)
        | _ -> raise (TypeError "Invalid indexing operation")
    }
    | expr LBRACKET expr RBRACKET LBRACKET expr RBRACKET { 
        match type_expr $1, type_expr $3, type_expr $6 with
        | TMatrix _, TInt, TInt -> MatrixAccess($1, $3, $6)
        | _ -> raise (TypeError "Invalid matrix access")
    }

vector_literal:
    | LBRACKET expr_list RBRACKET { 
        let elements = $2 in
        let first_type = match elements with
                         | [] -> raise (TypeError "Empty vector not allowed")
                         | e::_ -> type_expr e in
        let all_same_type = List.for_all (fun e -> types_compatible (type_expr e) first_type) elements in
        if not all_same_type then
            raise (TypeError "Vector elements must have the same type");
        VectorLit(elements)
    }

matrix_literal:
    | LBRACKET matrix_rows RBRACKET { 
        let rows = $2 in
        if rows = [] then
            raise (TypeError "Empty matrix not allowed");
        let row_lengths = List.map List.length rows in
        let first_length = List.hd row_lengths in
        if not (List.for_all (fun l -> l = first_length) row_lengths) then
            raise (TypeError "All matrix rows must have the same length");
        MatrixLit(rows)
    }

matrix_rows:
    | matrix_row { [$1] }
    | matrix_row COMMA matrix_rows { $1 :: $3 }

matrix_row:
    | LBRACKET non_empty_expr_list RBRACKET { $2 }

expr_list:
    | /* empty */ { [] }
    | non_empty_expr_list { $1 }

non_empty_expr_list:
    | expr { [$1] }
    | expr COMMA non_empty_expr_list { $1 :: $3 }

typed_ident:
    | BOOL_TYPE IDENTIFIER { ($2, TBool) }
    | INT_TYPE IDENTIFIER { ($2, TInt) }
    | FLOAT_TYPE IDENTIFIER { ($2, TFloat) }
    | VECTOR_TYPE LBRACKET INT_CONST RBRACKET IDENTIFIER { 
        if $3 <= 0 then
            raise (TypeError "Vector size must be positive");
        ($5, TVector($3)) 
    }
    | MATRIX_TYPE LBRACKET INT_CONST RBRACKET LBRACKET INT_CONST RBRACKET IDENTIFIER { 
        if $3 <= 0 || $6 <= 0 then
            raise (TypeError "Matrix dimensions must be positive");
        ($8, TMatrix($3, $6)) 
    }