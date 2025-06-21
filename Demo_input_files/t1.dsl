
{
  matrix[6][6] A := read_matrix("Demo_input_files/t1_A.txt", 6, 6);
  
  vector[6] b := [2.15, -0.97, 1.06, 0.34, 1.82, 3.4];
  
  matrix[6][6] A_T := A';
  
  matrix[6][6] A_TA := A_T * A;
  
  float det := determinant A_TA;
  
  if (det <> 0.0) then { 
    matrix[6][6] A_TA_inv := inv(A_TA);
    vector[6] A_Tb := A_T * b;
    vector [6] theta := A_TA_inv * A_Tb;
    Print(theta);
  }
  else {
    Print("Matrix is not invertible");
  }
}