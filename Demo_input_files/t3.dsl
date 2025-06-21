{
  matrix[4][1] v_matrix := read_matrix("Demo_input_files/t3_v.txt", 4, 1);
  

  vector[4] v := [v_matrix[0][0], v_matrix[1][0], v_matrix[2][0], v_matrix[3][0]];
  
  float sum_result := 0.0;
  
  int i := 0;

  for i := 0 to 3 do {
    sum_result := sum_result + v[i];
  }
  
  float ans := 2.5 * sum_result;
  
  Print(ans);
}