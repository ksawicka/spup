# Stefan van Dam
# Feburary 2016

# Function to reorder a list of realizations to satisfy LHS:
  # Each column and row of the stratified realizations are represented once

# Parameters: list.of.samples
  # This is a list of all the samples which has been sampled by genSample
  # when applying the lhs method.

# Returns:
  # It returns the same list of samples, but then reordered to satisfy the LHS conditions

lhs <- function(list.of.samples) {

  # CHECK if: all samples have equal strata
  q <- c()

  for (i in 1:length(list.of.samples)) {
    q.sample <- list.of.samples[[i]][[3]]
    q <- append(q, q.sample)
  }

  if (!all(q == q[1])) {
    stop("All samples should have equal number of strata")
  }

  q <- q[1]

  lhs.dim <- matrix(NA, nrow = q, ncol = length(list.of.samples))

  for (i in 1:length(list.of.samples)) {
    a <- sample(1:q)
    lhs.dim[,i] <- a
  }

  list.of.matrices <- list(length(list.of.samples))

  for (i in 1:length(list.of.samples)) {
    if (ncol(list.of.samples[[i]][[1]]) == q) {
      sample.matrix <- list.of.samples[[i]][[1]]
      new.matrix <- matrix(NA, nrow = nrow(sample.matrix), ncol = 0)

      for (j in 1:nrow(lhs.dim)) {
        new.matrix <- cbind(new.matrix, sample.matrix[,lhs.dim[j,i]])
      }
      class(new.matrix) <- c(class(list.of.samples[[i]])[1], class(new.matrix))
      list.of.matrices[i] <- list(new.matrix)
    } else {
      new.matrix <- list.of.samples[[i]][[1]]

      for (k in 1:nrow(new.matrix)) {
        row.k <- matrix(new.matrix[k,], ncol = q)

        if (all(is.na(row.k))) {
          new.matrix[k,] <- as.numeric(row.k)
        } else {
          temp.matrix <- matrix(NA, nrow = (ncol(new.matrix)/q), ncol = 0)

          for (j in 1:nrow(lhs.dim)) {
            temp.matrix <- cbind(temp.matrix, row.k[,lhs.dim[j,i]])
          }
          temp.matrix <- as.numeric(temp.matrix)
          new.matrix[k,] <- temp.matrix
        }
      }
      class(new.matrix) <- c(class(list.of.samples[[i]])[1], class(new.matrix))
      list.of.matrices[i] <- list(new.matrix)
    }
  }
  return(list.of.matrices)
}
