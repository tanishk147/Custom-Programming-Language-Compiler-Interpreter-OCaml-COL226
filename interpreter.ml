(* filepath: /Users/tanishkumar/Desktop/ College_Stuff/Code/COL_226/Assignment_5/interpreter.ml *)
open Ast


(* Exception for runtime errors *)
exception RuntimeError of string

(* Value types that can be produced by evaluating expressions *)
type value =
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VVector of value list
  | VMatrix of value list list
  | VString of string  

(* Environment to store variable bindings *)
type environment = (string, value) Hashtbl.t

(* Create a new empty environment *)
let create_env () = Hashtbl.create 64

(* Helper function to convert a value to string for printing *)
let rec string_of_value = function
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VString s -> "\"" ^ s ^ "\""
  | VVector vs -> 
      "[" ^ String.concat ", " (List.map string_of_value vs) ^ "]"
  | VMatrix rows ->
      "[" ^ String.concat "; " 
        (List.map (fun row -> 
          "[" ^ String.concat ", " (List.map string_of_value row) ^ "]") rows) 
        ^ "]"

(* Helper function to check if two values are equal *)
let rec values_equal v1 v2 =
  match v1, v2 with
  | VBool b1, VBool b2 -> b1 = b2
  | VInt i1, VInt i2 -> i1 = i2
  | VFloat f1, VFloat f2 -> f1 = f2
  | VString s1, VString s2 -> s1 = s2 
  | VVector vs1, VVector vs2 ->
      (try List.for_all2 values_equal vs1 vs2
       with Invalid_argument _ -> false)
  | VMatrix rows1, VMatrix rows2 ->
      (try List.for_all2 (fun r1 r2 -> 
        List.for_all2 values_equal r1 r2) rows1 rows2
       with Invalid_argument _ -> false)
  | _, _ -> false

(* Helper function to ensure a value is a vector of specified size *)
let ensure_vector size v =
  match v with
  | VVector vs when List.length vs = size -> vs
  | VVector vs -> 
      raise (RuntimeError (Printf.sprintf "Expected vector of size %d, got %d" 
             size (List.length vs)))
  | _ -> raise (RuntimeError "Expected a vector")

(* Helper function to ensure a value is a matrix of specified dimensions *)
let ensure_matrix rows cols v =
  match v with
  | VMatrix m when List.length m = rows && 
                   List.for_all (fun r -> List.length r = cols) m -> m
  | VMatrix m -> 
      raise (RuntimeError (Printf.sprintf "Expected matrix of size %dx%d" rows cols))
  | _ -> raise (RuntimeError "Expected a matrix")

(* Helper function to get dimensions of a matrix *)
let matrix_dimensions = function
  | VMatrix m -> 
      if m = [] then (0, 0)
      else (List.length m, List.length (List.hd m))
  | _ -> raise (RuntimeError "Expected a matrix")

(* Helper function to get size of a vector *)
let vector_size = function
  | VVector v -> List.length v
  | _ -> raise (RuntimeError "Expected a vect`or")



(* Evaluate binary operations *)
let rec eval_binop v1 op v2 =
  match (v1, op, v2) with
  (* Integer operations *)
  | (VInt i1, Add, VInt i2) -> VInt (i1 + i2)
  | (VInt i1, Sub, VInt i2) -> VInt (i1 - i2)
  | (VInt i1, Mul, VInt i2) -> VInt (i1 * i2)
  | (VInt i1, Div, VInt i2) -> 
      if i2 = 0 then raise (RuntimeError "Division by zero")
      else VInt (i1 / i2)
  | (VInt i1, Mod, VInt i2) -> 
      if i2 = 0 then raise (RuntimeError "Modulo by zero")
      else VInt (i1 mod i2)
  
  (* Float operations *)
  | (VFloat f1, Add, VFloat f2) -> VFloat (f1 +. f2)
  | (VFloat f1, Sub, VFloat f2) -> VFloat (f1 -. f2)
  | (VFloat f1, Mul, VFloat f2) -> VFloat (f1 *. f2)
  | (VFloat f1, Div, VFloat f2) -> 
      if f2 = 0.0 then raise (RuntimeError "Division by zero")
      else VFloat (f1 /. f2)
      
  (* Mixed numeric operations *)
  | (VInt i1, Add, VFloat f2) -> VFloat ((float_of_int i1) +. f2)
  | (VFloat f1, Add, VInt i2) -> VFloat (f1 +. (float_of_int i2))
  | (VInt i1, Sub, VFloat f2) -> VFloat ((float_of_int i1) -. f2)
  | (VFloat f1, Sub, VInt i2) -> VFloat (f1 -. (float_of_int i2))
  | (VInt i1, Mul, VFloat f2) -> VFloat ((float_of_int i1) *. f2)
  | (VFloat f1, Mul, VInt i2) -> VFloat (f1 *. (float_of_int i2))
  | (VInt i1, Div, VFloat f2) -> 
      if f2 = 0.0 then raise (RuntimeError "Division by zero")
      else VFloat ((float_of_int i1) /. f2)
  | (VFloat f1, Div, VInt i2) -> 
      if i2 = 0 then raise (RuntimeError "Division by zero")
      else VFloat (f1 /. (float_of_int i2))
  
  (* Vector operations *)
  | (VVector v1, Add, VVector v2) ->
      if List.length v1 <> List.length v2 then
        raise (RuntimeError "Vector dimensions don't match for addition")
      else
        VVector (List.map2 (fun a b -> eval_binop a Add b) v1 v2)
  | (VVector v1, Sub, VVector v2) ->
      if List.length v1 <> List.length v2 then
        raise (RuntimeError "Vector dimensions don't match for subtraction")
      else
        VVector (List.map2 (fun a b -> eval_binop a Sub b) v1 v2)
  
  (* Matrix operations *)
  | (VMatrix m1, Add, VMatrix m2) ->
      let (rows1, cols1) = matrix_dimensions (VMatrix m1) in
      let (rows2, cols2) = matrix_dimensions (VMatrix m2) in
      if rows1 <> rows2 || cols1 <> cols2 then
        raise (RuntimeError "Matrix dimensions don't match for addition")
      else
        VMatrix (List.map2 (fun r1 r2 -> 
          List.map2 (fun a b -> eval_binop a Add b) r1 r2) m1 m2)
  | (VMatrix m1, Sub, VMatrix m2) ->
      let (rows1, cols1) = matrix_dimensions (VMatrix m1) in
      let (rows2, cols2) = matrix_dimensions (VMatrix m2) in
      if rows1 <> rows2 || cols1 <> cols2 then
        raise (RuntimeError "Matrix dimensions don't match for subtraction")
      else
        VMatrix (List.map2 (fun r1 r2 -> 
          List.map2 (fun a b -> eval_binop a Sub b) r1 r2) m1 m2)
  
  (* Comparison operations *)
  | (_, Eq, _) -> VBool (values_equal v1 v2)
  | (_, Neq, _) -> VBool (not (values_equal v1 v2))
  | (VInt i1, Lt, VInt i2) -> VBool (i1 < i2)
  | (VInt i1, Gt, VInt i2) -> VBool (i1 > i2)
  | (VInt i1, Leq, VInt i2) -> VBool (i1 <= i2)
  | (VInt i1, Geq, VInt i2) -> VBool (i1 >= i2)
  | (VFloat f1, Lt, VFloat f2) -> VBool (f1 < f2)
  | (VFloat f1, Gt, VFloat f2) -> VBool (f1 > f2)
  | (VFloat f1, Leq, VFloat f2) -> VBool (f1 <= f2)
  | (VFloat f1, Geq, VFloat f2) -> VBool (f1 >= f2)
  | (VInt i1, Lt, VFloat f2) -> VBool ((float_of_int i1) < f2)
  | (VFloat f1, Lt, VInt i2) -> VBool (f1 < (float_of_int i2))
  | (VInt i1, Gt, VFloat f2) -> VBool ((float_of_int i1) > f2)
  | (VFloat f1, Gt, VInt i2) -> VBool (f1 > (float_of_int i2))
  | (VInt i1, Leq, VFloat f2) -> VBool ((float_of_int i1) <= f2)
  | (VFloat f1, Leq, VInt i2) -> VBool (f1 <= (float_of_int i2))
  | (VInt i1, Geq, VFloat f2) -> VBool ((float_of_int i1) >= f2)
  | (VFloat f1, Geq, VInt i2) -> VBool (f1 >= (float_of_int i2))
  
  (* Logical operations *)
  | (VBool b1, And, VBool b2) -> VBool (b1 && b2)
  | (VBool b1, Or, VBool b2) -> VBool (b1 || b2)
  
  | (_, _, _) -> 
      raise (RuntimeError (Printf.sprintf "Invalid operands for binary operation"))

(* Evaluate unary operations *)
let eval_unop op v =
  match op, v with
  | Not, VBool b -> VBool (not b)
  | Neg, VInt i -> VInt (-i)
  | Neg, VFloat f -> VFloat (-.f)
  | Neg, VVector elems -> 
      VVector (List.map (fun e -> match e with
                          | VInt i -> VInt (-i)
                          | VFloat f -> VFloat (-.f)
                          | _ -> raise (RuntimeError "Vector elements must be numeric")) elems)
  | Neg, VMatrix rows ->
      VMatrix (List.map (fun row -> 
                 List.map (fun e -> match e with
                             | VInt i -> VInt (-i)
                             | VFloat f -> VFloat (-.f)
                             | _ -> raise (RuntimeError "Matrix elements must be numeric")) row) rows)
  | Sqrt, VInt i -> VFloat (sqrt (float_of_int i))
  | Sqrt, VFloat f -> 
      if f < 0.0 then
        raise (RuntimeError "Cannot take square root of negative number")
      else
        VFloat (sqrt f)
  | _ -> raise (RuntimeError "Invalid unary operation")


(* Transpose a matrix or vector *)
let transpose v =
  match v with
  | VVector vs ->
      (* Vector transpose is just the vector itself *)
      v
  | VMatrix rows ->
      if rows = [] then VMatrix []
      else
        let cols = List.length (List.hd rows) in
        let transposed = Array.make_matrix cols (List.length rows) (List.hd (List.hd rows)) in
        List.iteri (fun i row ->
          List.iteri (fun j elem ->
            transposed.(j).(i) <- elem
          ) row
        ) rows;
        VMatrix (Array.to_list (Array.map Array.to_list transposed))
  | _ -> raise (RuntimeError "Transpose requires a matrix or vector")

(* Calculate determinant of a matrix *)
let rec determinant v =
  match v with

  | VMatrix rows ->
      let n = List.length rows in
      if n = 0 then VFloat 0.0
      else if n <> List.length (List.hd rows) then
        raise (RuntimeError "Determinant requires a square matrix")
      else if n = 1 then
        match List.hd (List.hd rows) with
        | VFloat f -> VFloat f
        | VInt i -> VFloat (float_of_int i)
        | _ -> raise (RuntimeError "Matrix elements must be numeric")
      else if n = 2 then
        let a = match List.nth (List.nth rows 0) 0 with
                | VFloat f -> f
                | VInt i -> float_of_int i
                | _ -> raise (RuntimeError "Matrix elements must be numeric") in
        let b = match List.nth (List.nth rows 0) 1 with
                | VFloat f -> f
                | VInt i -> float_of_int i
                | _ -> raise (RuntimeError "Matrix elements must be numeric") in
        let c = match List.nth (List.nth rows 1) 0 with
                | VFloat f -> f
                | VInt i -> float_of_int i
                | _ -> raise (RuntimeError "Matrix elements must be numeric") in
        let d = match List.nth (List.nth rows 1) 1 with
                | VFloat f -> f
                | VInt i -> float_of_int i
                | _ -> raise (RuntimeError "Matrix elements must be numeric") in
        VFloat (a *. d -. b *. c)
      else
        (* For larger matrices, use cofactor expansion along first row *)
        let first_row = List.hd rows in
        let rest_rows = List.tl rows in
        
        let rec cofactor_sum i acc =
          if i >= n then acc
          else
            let minor = List.map (fun row -> 
              List.filteri (fun j _ -> j <> i) row
            ) rest_rows in
            let cofactor = match List.nth first_row i with
                          | VFloat f -> f
                          | VInt i -> float_of_int i
                          | _ -> raise (RuntimeError "Matrix elements must be numeric") in
            let sign = if i mod 2 = 0 then 1.0 else -1.0 in
            let minor_det = match determinant (VMatrix minor) with
                           | VFloat f -> f
                           | _ -> raise (RuntimeError "Unexpected determinant result") in
            cofactor_sum (i + 1) (acc +. sign *. cofactor *. minor_det)
        in
        VFloat (cofactor_sum 0 0.0)
  | _ -> raise (RuntimeError "Determinant requires a matrix")


let inverse v =
  match v with
  | VMatrix rows ->
      let n = List.length rows in
      if n = 0 then raise (RuntimeError "Cannot invert empty matrix")
      else if n <> List.length (List.hd rows) then
        raise (RuntimeError "Matrix must be square to be inverted")
      else
        (* Calculate determinant and check if matrix is invertible *)
        let det_val = match determinant v with
          | VFloat f -> f
          | _ -> raise (RuntimeError "Unexpected determinant result") in
        
        if abs_float det_val < 1e-10 then
          raise (RuntimeError "Matrix is not invertible (determinant is zero)")
        else
          (* For a 1x1 matrix, inverse is trivial *)
          if n = 1 then
            match List.hd (List.hd rows) with
            | VFloat f -> VMatrix [[VFloat (1.0 /. f)]]
            | VInt i -> VMatrix [[VFloat (1.0 /. float_of_int i)]]
            | _ -> raise (RuntimeError "Matrix elements must be numeric")
          else
            (* Calculate the cofactor matrix *)
            let cofactor_matrix = Array.make_matrix n n (VFloat 0.0) in
            
            for i = 0 to n - 1 do
              for j = 0 to n - 1 do
                (* Create the minor matrix by removing row i and column j *)
                let minor = List.mapi (fun row_idx row -> (row_idx, row)) rows |>
                            List.filter (fun (row_idx, _) -> row_idx <> i) |>
                            List.map (fun (_, row) -> 
                              List.mapi (fun col_idx elem -> (col_idx, elem)) row |>
                              List.filter (fun (col_idx, _) -> col_idx <> j) |>
                              List.map snd) in
                
                (* Calculate determinant of minor *)
                let minor_det = match determinant (VMatrix minor) with
                                | VFloat f -> f
                                | _ -> raise (RuntimeError "Unexpected determinant result") in
                
                (* Calculate the cofactor with correct sign *)
                let sign = if (i + j) mod 2 = 0 then 1.0 else -1.0 in
                cofactor_matrix.(i).(j) <- VFloat (sign *. minor_det)
              done
            done;
            
            (* Transpose the cofactor matrix to get the adjugate *)
            let adjugate = transpose (VMatrix (Array.to_list (Array.map Array.to_list cofactor_matrix))) in
            
            (* Divide by determinant to get inverse *)
            match adjugate with
            | VMatrix adj_rows ->
                VMatrix (List.map (fun row ->
                  List.map (fun elem ->
                    match elem with
                    | VFloat f -> VFloat (f /. det_val)
                    | VInt i -> VFloat (float_of_int i /. det_val)
                    | _ -> raise (RuntimeError "Matrix elements must be numeric")
                  ) row
                ) adj_rows)
            | _ -> raise (RuntimeError "Unexpected result during inverse calculation")
  | _ -> raise (RuntimeError "Inverse requires a matrix")



(* Calculate dot product of two vectors *)
let dot_product v1 v2 =
  match (v1, v2) with
  | (VVector vs1, VVector vs2) ->
      if List.length vs1 <> List.length vs2 then
        raise (RuntimeError "Vectors must have same dimension for dot product")
      else
        let products = List.map2 (fun a b ->
          match (a, b) with
          | (VFloat f1, VFloat f2) -> f1 *. f2
          | (VInt i1, VInt i2) -> float_of_int (i1 * i2)
          | (VFloat f1, VInt i2) -> f1 *. float_of_int i2
          | (VInt i1, VFloat f2) -> float_of_int i1 *. f2
          | (_, _) -> raise (RuntimeError "Vector elements must be numeric")
        ) vs1 vs2 in
        VFloat (List.fold_left (+.) 0.0 products)
  | (_, _) -> raise (RuntimeError "Dot product requires two vectors")

(* Multiply two matrices *)
let matrix_mult m1 m2 =
  match (m1, m2) with
  | (VMatrix rows1, VMatrix rows2) ->
      if rows1 = [] || rows2 = [] then VMatrix []
      else
        let rows_m1 = List.length rows1 in
        let cols_m1 = List.length (List.hd rows1) in
        let rows_m2 = List.length rows2 in
        let cols_m2 = List.length (List.hd rows2) in
        
        if cols_m1 <> rows_m2 then
          raise (RuntimeError "Matrix dimensions incompatible for multiplication")
        else
          (* Convert to arrays for easier access *)
          let arr1 = Array.of_list (List.map Array.of_list rows1) in
          let arr2 = Array.of_list (List.map Array.of_list rows2) in
          
          (* Create result matrix *)
          let result = Array.make_matrix rows_m1 cols_m2 (VFloat 0.0) in
          
          (* Perform matrix multiplication *)
          for i = 0 to rows_m1 - 1 do
            for j = 0 to cols_m2 - 1 do
              let sum = ref (VFloat 0.0) in
              for k = 0 to cols_m1 - 1 do
                let prod = match (arr1.(i).(k), arr2.(k).(j)) with
                          | (VFloat f1, VFloat f2) -> VFloat (f1 *. f2)
                          | (VInt i1, VInt i2) -> VInt (i1 * i2)
                          | (VFloat f1, VInt i2) -> VFloat (f1 *. float_of_int i2)
                          | (VInt i1, VFloat f2) -> VFloat (float_of_int i1 *. f2)
                          | (_, _) -> raise (RuntimeError "Matrix elements must be numeric") in
                sum := eval_binop !sum Add prod
              done;
              result.(i).(j) <- !sum
            done
          done;
          
          VMatrix (Array.to_list (Array.map Array.to_list result))
  | (_, _) -> raise (RuntimeError "Matrix multiplication requires two matrices")

(* Multiply a matrix by a vector *)
let matrix_vector_mult m v =
  match (m, v) with
  | (VMatrix rows, VVector vs) ->
      if rows = [] then VVector []
      else
        let cols_m = List.length (List.hd rows) in
        let rows_m = List.length rows in
        let vec_size = List.length vs in
        
        if cols_m <> vec_size then
          raise (RuntimeError "Matrix and vector dimensions incompatible for multiplication")
        else
          (* Convert to arrays for easier access *)
          let arr_m = Array.of_list (List.map Array.of_list rows) in
          let arr_v = Array.of_list vs in
          
          (* Create result vector *)
          let result = Array.make rows_m (VFloat 0.0) in
          
          (* Perform matrix-vector multiplication *)
          for i = 0 to rows_m - 1 do
            let sum = ref (VFloat 0.0) in
            for j = 0 to cols_m - 1 do
              let prod = match (arr_m.(i).(j), arr_v.(j)) with
                        | (VFloat f1, VFloat f2) -> VFloat (f1 *. f2)
                        | (VInt i1, VInt i2) -> VInt (i1 * i2)
                        | (VFloat f1, VInt i2) -> VFloat (f1 *. float_of_int i2)
                        | (VInt i1, VFloat f2) -> VFloat (float_of_int i1 *. f2)
                        | (_, _) -> raise (RuntimeError "Elements must be numeric") in
              sum := eval_binop !sum Add prod
            done;
            result.(i) <- !sum
          done;
          
          VVector (Array.to_list result)
  | (_, _) -> raise (RuntimeError "Matrix-vector multiplication requires a matrix and a vector")

(* Main evaluation function for expressions *)
let rec eval_expr env = function
  | BoolLit b -> VBool b
  | IntLit i -> VInt i
  | FloatLit f -> VFloat f
  | StringLit s -> VString s
  | Var id -> 
      (try Hashtbl.find env id
       with Not_found -> raise (RuntimeError ("Undefined variable: " ^ id)))
  | VectorLit es -> 
      VVector (List.map (eval_expr env) es)
  | MatrixLit rows ->
      VMatrix (List.map (List.map (eval_expr env)) rows)
  | BinOp(e1, op, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      eval_binop v1 op v2
  | UnOp(op, e) ->
      let v = eval_expr env e in
      eval_unop op v
  | Transpose e ->
      let v = eval_expr env e in
      transpose v
  | Determinant e ->
      let v = eval_expr env e in
      determinant v
  | DotProduct(e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      dot_product v1 v2
  | MatrixMult(e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      matrix_mult v1 v2
  | MatrixVectorMult(e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      matrix_vector_mult v1 v2
  | IndexAccess(expr, idx) ->(
      let v = eval_expr env expr in
      let idx_value = eval_expr env idx in
      match v, idx_value with
      | VVector vec, VInt i ->
          if i < 0 || i >= List.length vec then
            raise (RuntimeError "Vector index out of bounds")
          else
            List.nth vec i
      | VMatrix mat, VInt i ->
          if i < 0 || i >= List.length mat then
            raise (RuntimeError "Matrix row index out of bounds")
          else
            VVector (List.nth mat i)
      | _, _ -> raise (RuntimeError "Index access requires a vector or matrix and integer index"))
  | MatrixAccess(expr, row_expr, col_expr) ->
      (let row_val = eval_expr env row_expr in
      let col_val = eval_expr env col_expr in
      let v = eval_expr env expr in
      match v, row_val, col_val with
      | VMatrix mat, VInt row_idx, VInt col_idx ->
          if row_idx < 0 || row_idx >= List.length mat then
            raise (RuntimeError "Matrix row index out of bounds")
          else
            let row = List.nth mat row_idx in
            if col_idx < 0 || col_idx >= List.length row then
              raise (RuntimeError "Matrix column index out of bounds")
            else
              List.nth row col_idx
      | _, _, _ -> raise (RuntimeError "Matrix access requires a matrix and integer indices"))

  | ReadMatrix(filename, rows, cols) ->
    (try
      let ic = open_in filename in
      try
        (* Read dimensions from first line *)
        let _ = input_line ic in
        
        (* Read the matrix data *)
        let data_line = input_line ic in
        close_in ic;
        
        (* Remove all brackets and spaces *)
        let data_clean = Str.global_replace (Str.regexp "\\[\\|\\]\\| ") "" data_line in
        
        (* Split by commas to get all elements in a flat list *)
        let all_elements = Str.split (Str.regexp ",") data_clean in
        
        (* Convert all elements to floats *)
        let all_floats = List.map (fun s -> VFloat (float_of_string s)) all_elements in
        
        (* Check if we have the right number of elements *)
        if List.length all_floats <> rows * cols then
          raise (RuntimeError (Printf.sprintf "Matrix has %d elements, expected %d"
                               (List.length all_floats) (rows * cols)));
        
        (* Group elements into rows *)
        let rec group_into_rows elements row_len acc =
          match elements with
          | [] -> List.rev acc
          | _ -> 
              let (row, rest) = List.fold_left 
                (fun (row, rest) i -> 
                  if List.length row < row_len && rest <> [] then
                    (List.hd rest :: row, List.tl rest)
                  else
                    (row, rest)
                ) ([], elements) (List.init row_len (fun i -> i))
              in
              group_into_rows rest row_len ((List.rev row) :: acc)
        in
        
        let matrix = group_into_rows all_floats cols [] in
        
        (* Verify dimensions *)
        if List.length matrix <> rows then
          raise (RuntimeError (Printf.sprintf "Matrix has %d rows, expected %d" 
                               (List.length matrix) rows));
        
        VMatrix matrix
        
      with End_of_file ->
        close_in ic;
        raise (RuntimeError "Unexpected end of file while reading matrix")
    with
    | Sys_error msg -> raise (RuntimeError ("Error reading matrix file: " ^ msg))
    | Failure msg -> raise (RuntimeError ("Error parsing matrix data: " ^ msg))  )
  | Index(expr, idx) ->
        eval_expr env (IndexAccess(expr, idx))
  | MatrixIndex(expr, row_idx, col_idx) ->
        eval_expr env (MatrixAccess(expr, row_idx, col_idx))
  | Inverse e ->
      let v = eval_expr env e in
      inverse v




(* Execute a command in the given environment *)
let rec eval_command env = function
  | Skip -> ()
  | Seq cmds -> 
      List.iter (eval_command env) cmds
  | Declare(id, ty, init_opt) ->
      (match init_opt with
       | Some e -> 
           let v = eval_expr env e in
           Hashtbl.replace env id v
       | None -> ())
  | Assign(id, e) ->
      let v = eval_expr env e in
      Hashtbl.replace env id v

  | IndexAssign(access_expr, value_expr) ->
      let value = eval_expr env value_expr in
      (match access_expr with
      | IndexAccess(expr, idx) ->
          let container = eval_expr env expr in
          let index = eval_expr env idx in
          (match container, index with
          | VVector vec, VInt i ->
              if i < 0 || i >= List.length vec then
                raise (RuntimeError "Vector index out of bounds")
              else
                let new_vec = 
                  List.mapi (fun j elem -> if j = i then value else elem) vec 
                in
                (match expr with
                | Var id -> Hashtbl.replace env id (VVector new_vec)
                | _ -> raise (RuntimeError "Cannot assign to non-variable indexed expression"))
          | VMatrix mat, VInt i ->
              if i < 0 || i >= List.length mat then
                raise (RuntimeError "Matrix row index out of bounds")
              else
                let new_mat = 
                  List.mapi (fun j row -> if j = i then 
                    (match value with
                    | VVector new_row -> new_row
                    | _ -> raise (RuntimeError "Cannot assign non-vector to matrix row"))
                  else row) mat 
                in
                (match expr with
                | Var id -> Hashtbl.replace env id (VMatrix new_mat)
                | _ -> raise (RuntimeError "Cannot assign to non-variable indexed expression"))
          | _, _ -> raise (RuntimeError "Index access requires a vector or matrix and integer index"))
      
      | MatrixAccess(expr, row_idx, col_idx) ->
        let r_idx = eval_expr env row_idx in
        let c_idx = eval_expr env col_idx in
        (match expr, r_idx, c_idx with
        | Var id, VInt row, VInt col ->
            (* Get the matrix from the environment *)
            let container = (match Hashtbl.find env id with
              | VMatrix mat -> mat
              | _ -> raise (RuntimeError "Expected a matrix for indexing")) in
            
            if row < 0 || row >= List.length container then
              raise (RuntimeError "Matrix row index out of bounds")
            else
              let matrix_row = List.nth container row in
              if col < 0 || col >= List.length matrix_row then
                raise (RuntimeError "Matrix column index out of bounds")
              else
                let new_mat = 
                  List.mapi (fun i r -> 
                    if i = row then
                      List.mapi (fun j elem -> 
                        if j = col then value else elem
                      ) r
                    else r
                  ) container
                in
                Hashtbl.replace env id (VMatrix new_mat)
            
      | IndexAccess(base_expr, row_expr), VInt col, _ ->
          (* This is for cases like A[i][j], where A[i] is a vector and we're setting A[i][j] *)
          (* First, validate that base_expr evaluates to a valid matrix variable *)
          (match base_expr with
          | Var matrix_id ->
              let row_idx = eval_expr env row_expr in
              match row_idx with
              | VInt row ->
                  (* Get the matrix from the environment *)
                  let matrix = try
                      match Hashtbl.find env matrix_id with
                      | VMatrix mat -> mat
                      | other -> 
                          raise (RuntimeError (Printf.sprintf 
                            "Expected a matrix for indexing '%s', but found %s" 
                            matrix_id (string_of_value other)))
                    with Not_found ->
                      raise (RuntimeError (Printf.sprintf "Variable '%s' is not defined" matrix_id))
                  in
                  
                  if row < 0 || row >= List.length matrix then
                    raise (RuntimeError (Printf.sprintf 
                      "Matrix row index %d out of bounds for matrix with %d rows" 
                      row (List.length matrix)))
                  else
                    let matrix_row = List.nth matrix row in
                    if col < 0 || col >= List.length matrix_row then
                      raise (RuntimeError (Printf.sprintf 
                        "Matrix column index %d out of bounds for row with %d columns" 
                        col (List.length matrix_row)))
                    else
                      let new_mat = 
                        List.mapi (fun i r -> 
                          if i = row then
                            List.mapi (fun j elem -> 
                              if j = col then value else elem
                            ) r
                          else r
                        ) matrix
                      in
                      Hashtbl.replace env matrix_id (VMatrix new_mat)
              | other -> 
                  raise (RuntimeError (Printf.sprintf 
                    "Matrix row index must be an integer, got: %s" 
                    (string_of_value other)))
          | _ -> raise (RuntimeError "Cannot assign to non-variable indexed expression"))
      
      | _, _, _ -> raise (RuntimeError "Cannot assign to non-variable indexed expression"))

      | Index(expr, idx) ->
          let container = eval_expr env expr in
          let index = eval_expr env idx in
          (match container, index with
          | VVector vec, VInt i ->
              if i < 0 || i >= List.length vec then
                raise (RuntimeError "Vector index out of bounds")
              else
                let new_vec = 
                  List.mapi (fun j elem -> if j = i then value else elem) vec 
                in
                (match expr with
                | Var id -> Hashtbl.replace env id (VVector new_vec)
                | _ -> raise (RuntimeError "Cannot assign to non-variable indexed expression"))
          | VMatrix mat, VInt i ->
              (* Same handling as IndexAccess *)
              if i < 0 || i >= List.length mat then
                raise (RuntimeError "Matrix row index out of bounds")
              else
                let new_mat = 
                  List.mapi (fun j row -> if j = i then 
                    (match value with
                    | VVector new_row -> new_row
                    | _ -> raise (RuntimeError "Cannot assign non-vector to matrix row"))
                  else row) mat 
                in
                (match expr with
                | Var id -> Hashtbl.replace env id (VMatrix new_mat)
                | _ -> raise (RuntimeError "Cannot assign to non-variable indexed expression"))
          | _, _ -> raise (RuntimeError "Index access requires a vector or matrix and integer index"))
      
      | MatrixIndex(expr, row_idx, col_idx) ->
          let container = eval_expr env expr in
          let r_idx = eval_expr env row_idx in
          let c_idx = eval_expr env col_idx in
          (match container, r_idx, c_idx with
          | VMatrix mat, VInt row, VInt col ->
              (* Same handling as MatrixAccess *)
              if row < 0 || row >= List.length mat then
                raise (RuntimeError "Matrix row index out of bounds")
              else
                let matrix_row = List.nth mat row in
                if col < 0 || col >= List.length matrix_row then
                  raise (RuntimeError "Matrix column index out of bounds")
                else
                  let new_mat = 
                    List.mapi (fun i r -> 
                      if i = row then
                        List.mapi (fun j elem -> 
                          if j = col then value else elem
                        ) r
                      else r
                    ) mat
                  in
                  (match expr with
                  | Var id -> Hashtbl.replace env id (VMatrix new_mat)
                  | _ -> raise (RuntimeError "Cannot assign to non-variable indexed expression"))
          | _, _, _ -> raise (RuntimeError "Matrix access requires a matrix and integer indices"))
      
      | _ -> raise (RuntimeError "Left side of assignment must be an index access")
      )

  | Print e ->  
      let v = eval_expr env e in
      print_endline (string_of_value v)

  | Input None ->
      () (* No variable to store input in *)
  | Input (Some (Var id)) ->
      let input_line = read_line () in
      let v = 
        try
          (* Try to parse as int *)
          VInt (int_of_string input_line)
        with Failure _ ->
          try
            (* Try to parse as float *)
            VFloat (float_of_string input_line)
          with Failure _ ->
            (* If not numeric, treat as string representation of bool *)
            if input_line = "true" then VBool true
            else if input_line = "false" then VBool false
            else raise (RuntimeError "8Invalid input format")
      in
      Hashtbl.replace env id v
  | Input _ ->
      raise (RuntimeError "Input must provide a variable reference")
  | IfThenElse(cond, then_cmd, else_cmd) ->
      let v = eval_expr env cond in
      (match v with
      | VBool true -> eval_command env then_cmd
      | VBool false -> eval_command env else_cmd
      | _ -> raise (RuntimeError "Condition must be a boolean"))
  | WhileLoop(cond, body) ->
      let rec loop () =
        let v = eval_expr env cond in
        match v with
        | VBool true -> 
            eval_command env body;
            loop ()
        | VBool false -> ()
        | _ -> raise (RuntimeError "Condition must be a boolean")
      in
      loop ()
  | ForLoop(id, start_expr, end_expr, body) ->
      let start_val = eval_expr env start_expr in
      let end_val = eval_expr env end_expr in
      match start_val, end_val with
      | VInt start, VInt end_val ->
          let rec loop i =
            if i <= end_val then begin
              Hashtbl.replace env id (VInt i);
              eval_command env body;
              loop (i + 1)
            end
          in
          loop start
      | _, _ -> raise (RuntimeError "For loop bounds must be integers")

      

(* The main interpreter function *)
let interpret program =
  let env = create_env () in
  eval_command env program