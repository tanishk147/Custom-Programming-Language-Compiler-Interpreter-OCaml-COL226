{
  matrix[3][2] A := read_matrix("Demo_input_files/t5_A.txt", 3, 2);
  matrix[2][3] B := read_matrix("Demo_input_files/t5_B.txt", 2, 3);
  
    
  matrix[3][3] C := A * B;
  
  Print("Result of matrix multiplication A * B:");
  Print(C);
  
  matrix[2][2] D := B * A;
  Print("Result of matrix multiplication B * A:");
  Print(D);
}