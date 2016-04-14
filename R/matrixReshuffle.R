# Stefan van Dam
# 08 March 2016

# Function to reshuffle the realizations drawn by genSample when Stratified sampling is used.
  # this is done to prevent biased data.

# Parameters: sm
  # this is the input matrix, which is the output of genSample

# Returns:
  # the same matrix. but then reshuffled, in order to prevent biased data.

matrixReshuffle <- function(sm) {
  shuffled_sm <- sample(sm)

  new_matrix <- matrix(shuffled_sm, nrow = nrow(sm), ncol = ncol(sm))
  return(new_matrix)
}

# Example
# t1 <- matrix(1:25, nrow = 5)

# t2 <- matrixReshuffle(t1)
