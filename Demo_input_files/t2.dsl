{
  matrix[4][5] A := read_matrix("Demo_input_files/t2_A.txt", 4, 5);
  matrix[4][5] B := read_matrix("Demo_input_files/t2_B.txt", 4, 5);
  matrix[5][4] D := read_matrix("Demo_input_files/t2_D.txt", 5, 4);
  
  vector[4] u := [1.0, 2.0, 3.0, 4.0];  // Replace with actual values from t2_u.txt
  
  matrix[4][5] C := A + B;
  
  matrix[4][4] E := C * D;
  
  float det := determinant(E);
  
  if (det <> 0.0) then {
    matrix[4][4] E_inverse := inv(E);
    vector[4] x := E_inverse * u;
    Print(x);
  }
  else {
    Print("Matrix is not invertible");
  }
}