{
  matrix[4][5] A := read_matrix("Demo_input_files/t4_A.txt", 4, 5);
  
  // Initialize variables
  float threshold := 0.001;  // 1e-3
  float sum_of_squares := 0.0;
  
  int i := 0;
  int j := 0;
  
  for i := 0 to 3 do {
    for j := 0 to 4 do {
      sum_of_squares := sum_of_squares + A[i][j] * A[i][j];
    }
  }
  
  float norm := sqrt(sum_of_squares);
  
  while (norm > threshold) do {
    for i := 0 to 3 do {
      for j := 0 to 4 do {
        A[i][j] := A[i][j] * 0.5;
      }
    }
    
    sum_of_squares := 0.0;
    for i := 0 to 3 do {
      for j := 0 to 4 do {
        sum_of_squares := sum_of_squares + A[i][j] * A[i][j];
      }
    }
    
    norm := sqrt(sum_of_squares);
  }
  
  Print(A);
}