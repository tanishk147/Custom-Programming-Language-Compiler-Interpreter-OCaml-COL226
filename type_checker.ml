open Ast

exception TypeError of string

let symbol_table = Hashtbl.create 64


let types_compatible t1 t2 =
  match t1, t2 with
  | TInt, TInt | TFloat, TFloat | TBool, TBool -> true
  | TInt, TFloat | TFloat, TInt -> true  
  | TVector n, TVector m when n = m -> true
  | TMatrix(a,b), TMatrix(c,d) when a = c && b = d -> true
  | _ -> false

let rec type_expr = function
  | BoolLit _ -> TBool
  | IntLit _ -> TInt
  | FloatLit _ -> TFloat
  | StringLit _ -> TString  
  | Var id -> 
      (try Hashtbl.find symbol_table id 
       with Not_found -> raise (TypeError ("Undefined variable: " ^ id)))
  | VectorLit es ->
      let dim = List.length es in
      if dim = 0 then
        raise (TypeError "Empty vector not allowed");
      let el_type = type_expr (List.hd es) in
      List.iter (fun e -> 
        if not (types_compatible (type_expr e) el_type) then 
          raise (TypeError (Printf.sprintf "Vector elements must be same type: found %s and %s" 
                 (string_of_type el_type) (string_of_type (type_expr e))))
      ) es;
      TVector(dim)
  | MatrixLit rows ->
      let row_count = List.length rows in
      if row_count = 0 then
        raise (TypeError "Empty matrix not allowed");
      let col_count = List.length (List.hd rows) in
      if col_count = 0 then
        raise (TypeError "Matrix cannot have empty rows");
      
      let first_element = List.hd (List.hd rows) in
      let el_type = type_expr first_element in
      
      (* Check all rows have same length and elements same type *)
      List.iter (fun row ->
        if List.length row <> col_count then
          raise (TypeError "All matrix rows must have the same length");
        List.iter (fun e -> 
          if not (types_compatible (type_expr e) el_type) then
            raise (TypeError (Printf.sprintf "Matrix elements must be same type: found %s and %s" 
                   (string_of_type el_type) (string_of_type (type_expr e))))
        ) row  
      ) rows;
      TMatrix(row_count, col_count)
  | BinOp(e1, op, e2) ->
      let t1 = type_expr e1 in
      let t2 = type_expr e2 in
      (match op with
      | Add | Sub -> 
        (match t1, t2 with
        | TInt, TInt -> TInt
        | TFloat, TFloat -> TFloat
        | TInt, TFloat -> TFloat   
        | TFloat, TInt -> TFloat   
        | TVector n, TVector m when n = m -> TVector n
        | TMatrix(a,b), TMatrix(c,d) when a = c && b = d -> TMatrix(a,b)
        | _ -> raise (TypeError (Printf.sprintf "Invalid operands for %s: %s and %s" 
                   (string_of_binop op) (string_of_type t1) (string_of_type t2))))
      | Mul | Div -> 
          (match t1, t2 with
          | TInt, TInt -> TInt
          | TFloat, TFloat -> TFloat
          | TInt, TFloat -> TFloat   
          | TFloat, TInt -> TFloat   
          | _ -> raise (TypeError (Printf.sprintf "Invalid operands for %s: %s and %s" 
                    (string_of_binop op) (string_of_type t1) (string_of_type t2))))
      | Mod -> 
          (match t1, t2 with
          | TInt, TInt -> TInt
          | _ -> raise (TypeError "Modulus operator requires integer operands"))
      | Eq | Neq | Lt | Gt | Leq | Geq -> 
          if types_compatible t1 t2 then TBool
          else raise (TypeError (Printf.sprintf "Cannot compare values of different types: %s and %s" 
                      (string_of_type t1) (string_of_type t2)))
      | And | Or -> 
          (match t1, t2 with
          | TBool, TBool -> TBool
          | _ -> raise (TypeError "Logical operators require boolean operands"))
      )
(* In type_checker.ml *)
  | UnOp(op, e) -> (
      match op with
      | Not -> 
          if type_expr e <> TBool then
              raise (TypeError "NOT operator requires boolean operand");
          TBool
      | Neg ->
          (match type_expr e with
          | TInt -> TInt
          | TFloat -> TFloat
          | TVector n -> TVector n
          | TMatrix(m, n) -> TMatrix(m, n)
          | _ -> raise (TypeError "Unary minus can only be applied to numeric types"))
      | Sqrt ->  (* This line was causing the error *)
          match type_expr e with
          | TInt -> TFloat  (* sqrt of int returns float *)
          | TFloat -> TFloat
          | _ -> raise (TypeError "Square root requires numeric operand"))
      
  | Transpose e ->
      (match type_expr e with
      | TMatrix(r,c) -> TMatrix(c,r)
      | TVector n -> TVector n  (* Vector transpose is the same vector *)
      | _ -> raise (TypeError "Transpose requires matrix or vector"))
  | DotProduct(e1, e2) ->
      (match (type_expr e1, type_expr e2) with
      | (TVector n, TVector m) when n = m -> TFloat
      | _ -> raise (TypeError "Dot product requires vectors of the same size"))
  | MatrixMult(e1, e2) ->
      (match (type_expr e1, type_expr e2) with
      | (TMatrix(r1,c1), TMatrix(r2,c2)) when c1 = r2 -> TMatrix(r1,c2)
      | _ -> raise (TypeError "Matrix multiplication dimensions are incompatible"))
  | MatrixVectorMult(e1, e2) ->
      (match (type_expr e1, type_expr e2) with
      | (TMatrix(r,c), TVector n) when c = n -> TVector r
      | _ -> raise (TypeError "Matrix-vector multiplication dimensions are incompatible"))
  | Determinant e ->
      (match type_expr e with
      | TMatrix(n,m) when n = m -> TFloat
      | _ -> raise (TypeError "Determinant requires square matrix"))
  | IndexAccess(expr, idx) ->
      let base_type = type_expr expr in
      let idx_type = type_expr idx in
      if idx_type <> TInt then 
        raise (TypeError "Index must be an integer");
      (match base_type with
      | TVector _ -> TFloat  (* Assuming vector elements are floats *)
      | TMatrix _ -> TVector(0)  (* Row of the matrix - dimension will be determined at runtime *)
      | _ -> raise (TypeError "Indexed access requires vector or matrix"))
  | MatrixAccess(expr, row, col) ->
      let base_type = type_expr expr in  
      let row_type = type_expr row in
      let col_type = type_expr col in
      if row_type <> TInt || col_type <> TInt then 
        raise (TypeError "Matrix indices must be integers");
      (match base_type with
      | TMatrix _ -> TFloat  (* Assuming matrix elements are floats *)
      | _ -> raise (TypeError "Matrix access requires a matrix"))

  | ReadMatrix(_, rows, cols) ->
      if rows <= 0 || cols <= 0 then
        raise (TypeError "Matrix dimensions must be positive");
      TMatrix(rows, cols)
  
  | Index(e1, e2) ->
      let base_type = type_expr e1 in
      let idx_type = type_expr e2 in
      if idx_type <> TInt then 
        raise (TypeError "Index must be an integer");
      (match base_type with
      | TVector _ -> TFloat  (* Assuming vector elements are floats *)
      | TMatrix _ -> TVector(0)  (* Row of the matrix - dimension will be determined at runtime *)
      | _ -> raise (TypeError "Index requires vector or matrix"))
      
  | MatrixIndex(e1, e2, e3) ->
      let base_type = type_expr e1 in
      let row_type = type_expr e2 in
      let col_type = type_expr e3 in
      if row_type <> TInt || col_type <> TInt then 
        raise (TypeError "Matrix indices must be integers");
      (match base_type with
      | TMatrix _ -> TFloat  (* Assuming matrix elements are floats *)
      | _ -> raise (TypeError "Matrix index requires a matrix"))

  | Inverse e ->
      match type_expr e with
      | TMatrix(n, m) when n = m -> TMatrix(n, m) 
      | TMatrix _ -> raise (TypeError "Matrix must be square to be inverted")
      | _ -> raise (TypeError "Inverse can only be applied to square matrices")


let typed_binop e1 op e2 =
  let t1 = type_expr e1 in
  let t2 = type_expr e2 in
  match op, t1, t2 with
  | Add, TInt, TInt | Sub, TInt, TInt | Mul, TInt, TInt | Div, TInt, TInt | Mod, TInt, TInt -> 
      BinOp(e1, op, e2)
  | Add, TFloat, TFloat | Sub, TFloat, TFloat | Mul, TFloat, TFloat | Div, TFloat, TFloat -> 
      BinOp(e1, op, e2)
  | Add, TInt, TFloat | Sub, TInt, TFloat | Mul, TInt, TFloat | Div, TInt, TFloat -> 
      BinOp(e1, op, e2)
  | Add, TFloat, TInt | Sub, TFloat, TInt | Mul, TFloat, TInt | Div, TFloat, TInt -> 
      BinOp(e1, op, e2)
  | Add, TVector n, TVector m | Sub, TVector n, TVector m when n = m -> 
      BinOp(e1, op, e2)
  | Add, TMatrix(a,b), TMatrix(c,d) | Sub, TMatrix(a,b), TMatrix(c,d) when a = c && b = d -> 
      BinOp(e1, op, e2)
  | Mul, TVector n, TVector m when n = m -> 
      DotProduct(e1, e2)
  | Mul, TMatrix(a,b), TMatrix(c,d) when b = c -> 
      MatrixMult(e1, e2)
  | Mul, TMatrix(a,b), TVector n when b = n -> 
      MatrixVectorMult(e1, e2)
  | _ -> raise (TypeError (Printf.sprintf "Invalid binary operation: %s %s %s" 
                (string_of_type t1) (string_of_binop op) (string_of_type t2)))

let comparison_op e1 op e2 =
  let t1 = type_expr e1 in
  let t2 = type_expr e2 in
  if types_compatible t1 t2 then
    BinOp(e1, op, e2)
  else
    raise (TypeError (Printf.sprintf "Cannot compare values of different types: %s and %s" 
            (string_of_type t1) (string_of_type t2)))

let logical_op e1 op e2 =
  let t1 = type_expr e1 in
  let t2 = type_expr e2 in
  match t1, t2 with
  | TBool, TBool -> BinOp(e1, op, e2)
  | _ -> raise (TypeError (Printf.sprintf "Logical operations require boolean operands, got %s and %s" 
                (string_of_type t1) (string_of_type t2)))

(* New function to type check commands *)
let rec type_check_command = function
  | Seq cmds -> 
      List.iter type_check_command cmds
  | Declare(id, ty, None) ->
      Hashtbl.add symbol_table id ty
  | Declare(id, ty, Some e) ->
      let expr_type = type_expr e in
      if types_compatible ty expr_type then
        Hashtbl.add symbol_table id ty
      else
        raise (TypeError (Printf.sprintf "Type mismatch in declaration: expected %s, got %s" 
               (string_of_type ty) (string_of_type expr_type)))
  | Assign(id, e) ->
      let var_type = 
        try Hashtbl.find symbol_table id 
        with Not_found -> raise (TypeError ("Undefined variable: " ^ id)) in
      let expr_type = type_expr e in
      if not (types_compatible var_type expr_type) then
        raise (TypeError (Printf.sprintf "Type mismatch in assignment: variable %s has type %s, expression has type %s" 
               id (string_of_type var_type) (string_of_type expr_type)))
  | Print e ->
      ignore (type_expr e)
  | Input None ->
      ()
  | Input (Some e) ->
      ignore (type_expr e)
  | IfThenElse(cond, then_cmd, else_cmd) ->
      let cond_type = type_expr cond in
      if cond_type <> TBool then
        raise (TypeError (Printf.sprintf "Condition must be boolean, got %s" 
               (string_of_type cond_type)));
      type_check_command then_cmd;
      type_check_command else_cmd
  | WhileLoop(cond, body) ->
      let cond_type = type_expr cond in
      if cond_type <> TBool then
        raise (TypeError (Printf.sprintf "Condition must be boolean, got %s" 
               (string_of_type cond_type)));
      type_check_command body
  | ForLoop(var, start, end_expr, body) ->
      let start_type = type_expr start in
      let end_type = type_expr end_expr in
      if start_type <> TInt || end_type <> TInt then
        raise (TypeError "For loop bounds must be integers");
      Hashtbl.add symbol_table var TInt;
      type_check_command body;
      Hashtbl.remove symbol_table var
  | Skip -> ()

  | IndexAssign(access_expr, value_expr) ->
      let access_type = type_expr access_expr in
      let value_type = type_expr value_expr in
      if not (types_compatible access_type value_type) then
        raise (TypeError (Printf.sprintf "Type mismatch in index assignment: expected %s, got %s" 
               (string_of_type access_type) (string_of_type value_type)))