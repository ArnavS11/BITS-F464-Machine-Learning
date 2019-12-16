nRows <- 4
nCols <- 4

mat <- matrix(c(1,1,1,1),nrow = 2, ncol = 2)

checkMatrixSum <- function(mat) {
  rows <- nrow(mat)
  cols <- ncol(mat)
  
  if(rows != cols) {
    print("Matrix is not a square matrix\n")
    return("Matrix is not square\n")
  }
  
  minim <- min(rows,cols)
  for(i in 1:minim) {
    sum1 <- 0
    for(j in 1:cols)
      sum1 = sum1 + mat[i,j]
      
    sum2 <- 0
    for(j in 1:rows)
      sum2 = sum2 + mat[j,i]
    
    if(sum2 != sum1) {
      print("Sums are not equal for row and column number ")
      print(i)
      return("No")
    }
  }
  
  print("Yes")
  return("Yes")
}

inverseMatrix <- function(mat) {
  detr <- det(mat)
  if(detr == 0)
    return("Matrix not invertible")
  return(solve(mat))
}

