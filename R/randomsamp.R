# Stefan van Dam
# 14-01-2016

# This function is a nested rnorm function
  # Added functionality: ignores NA values if the object to sample from contains them
  # Only used for spatial grid based sampling

# Parameters:
  # mu: the value in a cell of the spatial object
  # sigma: the corresponding sd of that cell.
  # n: the amount of realizations required by the user

# Returns:
  # a list of realizations

# Supporting function for:
  # genSample.R

randomSamp <- function(mu, sigma, n) {
  if(is.na(mu) || is.na(sigma)) {
    samples <- rep(NA, n)
  } else {
    samples <- rnorm(n, mu, sigma)
  }

  return(samples)
}
