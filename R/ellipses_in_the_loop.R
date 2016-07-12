# 
# mymodel <- function(a, b) {
#   a*b
# }

run <- function(model = mymodel, n, ...) {
  
  myrun <- numeric(n) # n cannot be higher than number of simulations of uncert var.
  for (i in 1:n) {
    myrun[i] <- model(...)
  }
  myrun
  
}

# a <- sample(1:10, 6, replace = TRUE)
# b <- 2
# c <- run(model = mymodel, n = 6, a = a , b = b)
