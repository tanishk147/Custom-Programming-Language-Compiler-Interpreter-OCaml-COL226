{
  matrix[2][2] A := read_matrix("Demo_input_files/t8_A.txt", 2, 2);
  
  Print("Matrix A:");
  Print(A);
  
  float det := determinant A;
  Print("Determinant of A:");
  Print(det);
  
  if (det <> 0.0) then {
    matrix[2][2] A_inv := inv(A);
    Print("Inverse of A:");
    Print(A_inv);
    
    matrix[2][2] I := A * A_inv;
    Print("A * A_inverse (should be identity):");
    Print(I);
  }
  else {
    Print("Matrix is not invertible (determinant is zero)");
  }
}
