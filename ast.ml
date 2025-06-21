(* ast.ml *)

type type_expr =
  | TBool
  | TInt
  | TFloat
  | TVector of int
  | TMatrix of int * int
  | TString

let type_to_string t = match t with
  | TBool   -> "TBool"
  | TInt   -> "TInt"
  | TFloat  -> "TFloat"
  | TVector n  -> "TVectir of " ^ string_of_int n
  | TMatrix(r,c) -> "TMatrix of " ^ string_of_int r ^ " * " ^ string_of_int c
  | TString -> "TString"

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Leq | Geq
  | And | Or

type unop =
  | Not
  | Neg 
  | Sqrt

type expr =
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | Var of string
  | VectorLit of expr list
  | MatrixLit of expr list list
  | BinOp of expr * binop * expr
  | UnOp of unop * expr
  | Transpose of expr
  | Determinant of expr
  | DotProduct of expr * expr
  | MatrixMult of expr * expr
  | MatrixVectorMult of expr * expr
  | IndexAccess of expr * expr
  | MatrixAccess of expr * expr * expr
  | StringLit of string
  | Index of expr * expr
  | MatrixIndex of expr * expr * expr
  | ReadMatrix of string * int * int
  | Inverse of expr
  | Sqrt of expr




type command =
  | Seq of command list
  | Declare of string * type_expr * expr option
  | Assign of string * expr
  | Print of expr
  | Input of expr option
  | IfThenElse of expr * command * command
  | WhileLoop of expr * command
  | ForLoop of string * expr * expr * command
  | Skip
  | IndexAssign of expr * expr 

(* Pretty printing functions *)
(* Pretty printing functions for exact AST representation *)
let rec string_of_expr = function
  | BoolLit b -> "BoolLit(" ^ string_of_bool b ^ ")"
  | IntLit i -> "IntLit(" ^ string_of_int i ^ ")"
  | FloatLit f -> "FloatLit(" ^ string_of_float f ^ ")"
  | Var id -> "Var(\"" ^ id ^ "\")"
  | VectorLit es -> 
      "VectorLit([" ^ String.concat "; " (List.map string_of_expr es) ^ "])"
  | MatrixLit rows ->
      "MatrixLit([" ^ 
      String.concat "; " 
        (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_expr row) ^ "]") rows) ^ 
      "])"
  | BinOp(e1, op, e2) ->
      "BinOp(" ^ string_of_expr e1 ^ ", " ^ string_of_binop op ^ ", " ^ string_of_expr e2 ^ ")"
  | UnOp(op, e) ->
      "UnOp(" ^ string_of_unop op ^ ", " ^ string_of_expr e ^ ")"
  | Transpose e ->
      "Transpose(" ^ string_of_expr e ^ ")"
  | Determinant e ->
      "Determinant(" ^ string_of_expr e ^ ")"
  | DotProduct(e1, e2) ->
      "DotProduct(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | MatrixMult(e1, e2) ->
      "MatrixMult(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | MatrixVectorMult(e1, e2) ->
      "MatrixVectorMult(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | IndexAccess(expr, idx) ->
    "IndexAccess(" ^ string_of_expr expr ^ ", " ^ string_of_expr idx ^ ")"
  | MatrixAccess(expr, row, col) ->
    "MatrixAccess(" ^ string_of_expr expr ^ ", " ^ string_of_expr row ^ ", " ^ string_of_expr col ^ ")"
  | StringLit s -> "StringLit(\"" ^ s ^ "\")"

  | ReadMatrix(filename, rows, cols) ->
      "ReadMatrix(\"" ^ filename ^ "\", " ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ")"
  | Index(expr, idx) ->
      "Index(" ^ string_of_expr expr ^ ", " ^ string_of_expr idx ^ ")"
  | MatrixIndex(expr, row, col) ->
      "MatrixIndex(" ^ string_of_expr expr ^ ", " ^ string_of_expr row ^ ", " ^ string_of_expr col ^ ")"
  | Inverse e -> "Inverse(" ^ string_of_expr e ^ ")"
  | UnOp(Sqrt, e) -> "Sqrt(" ^ string_of_expr e ^ ")"



and string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Leq -> "Leq"
  | Geq -> "Geq"
  | And -> "And"
  | Or -> "Or"

and string_of_unop = function
  | Not -> "Not"
  | Neg -> "Neg"
  | Sqrt -> "Sqrt"

let rec string_of_command = function
  | Seq cmds -> 
      "Seq([" ^ String.concat "; " (List.map string_of_command cmds) ^ "])"
  | Declare(id, ty, None) ->
      "Declare(\"" ^ id ^ "\", " ^ string_of_type ty ^ ", None)"
  | Declare(id, ty, Some e) ->
      "Declare(\"" ^ id ^ "\", " ^ string_of_type ty ^ ", Some(" ^ string_of_expr e ^ "))"
  | Assign(id, e) ->
      "Assign(\"" ^ id ^ "\", " ^ string_of_expr e ^ ")"
  | Print e ->
      "Print(" ^ string_of_expr e ^ ")"
  | Input None ->
      "Input(None)"
  | Input (Some e) ->
      "Input(Some(" ^ string_of_expr e ^ "))"
  | IfThenElse(cond, then_cmd, else_cmd) ->
      "IfThenElse(" ^ string_of_expr cond ^ ", " ^ 
      string_of_command then_cmd ^ ", " ^ 
      string_of_command else_cmd ^ ")"
  | WhileLoop(cond, body) ->
      "WhileLoop(" ^ string_of_expr cond ^ ", " ^ string_of_command body ^ ")"
  | ForLoop(var, start, end_expr, body) ->
      "ForLoop(\"" ^ var ^ "\", " ^ string_of_expr start ^ ", " ^ 
      string_of_expr end_expr ^ ", " ^ string_of_command body ^ ")"
  | Skip -> "Skip"

  | IndexAssign(access_expr, value_expr) ->
      "IndexAssign(" ^ string_of_expr access_expr ^ ", " ^ string_of_expr value_expr ^ ")"

and string_of_type = function
  | TBool -> "TBool"
  | TInt -> "TInt"
  | TFloat -> "TFloat"
  | TVector n -> "TVector(" ^ string_of_int n ^ ")"
  | TMatrix(r, c) -> "TMatrix(" ^ string_of_int r ^ ", " ^ string_of_int c ^ ")"
  | TString -> "TString"