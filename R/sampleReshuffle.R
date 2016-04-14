# Stefan van Dam
# 1 February 2016

sampleReshuffle <- function(sample_matrix) {
  new_matrix <- matrix(NA, nrow = nrow(sample_matrix), ncol = ncol(sample_matrix))

  n <- length(sample_matrix)
  q <- ncol(sample_matrix)

  for (i in 1:ncol(sample_matrix)) {
    # select and remove last n/q/q rows
    matrix_tail <- tail(sample_matrix, n/q/q)
    sample_matrix <- head(sample_matrix, -(n/q/q))

    # make matrix tail numeric
    matrix_tail <- as.numeric(matrix_tail)

    # put the selected rows in the new matrix
    new_matrix[,i] <- matrix_tail
  }
  return(new_matrix)
}

# t1 <- matrix(1:100, 20, 5)

# sampleReshuffle(t1)
