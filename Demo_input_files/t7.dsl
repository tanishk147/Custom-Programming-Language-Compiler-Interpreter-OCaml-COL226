{
  matrix[3][3] A := read_matrix("Demo_input_files/t7_A.txt", 3, 3);
  
  matrix[3][1] v_matrix := read_matrix("Demo_input_files/t7_v.txt", 3, 1);
  
  vector[3] v := [v_matrix[0][0], v_matrix[1][0], v_matrix[2][0]];
  
  matrix[3][3] C := A;  // Start with a copy of A
  
  int j := 0;
  for j := 0 to 2 do {
    C[0][j] := A[0][j] + v[0];
    C[1][j] := A[1][j] + v[1];
    C[2][j] := A[2][j] + v[2];
  }
  
  Print("Original matrix A:");
  Print(A);
  
  Print("Vector v:");
  Print(v);
  
  Print("Result of adding v to each column of A:");
  Print(C);
}