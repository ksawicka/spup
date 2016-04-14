 setClass("nummar",
           slots = list(uncertain = "logical",
                        distribution = "character",
                        paramdist = "numeric",
                        meanfun = "function"))

 meanfun <- function(distribution, paramdist) {
      if (distribution == "norm") {
        m <- paramdist[1]
      } else if (distribution == "beta") {
        a <- paramdist[1]
        b <- paramdist[2]
        m <- a/(a+b)
      }
      return(m)
    }
 defnummar <- function(uncertain, distribution, paramdist)
{
  # here check if paramdist match distribution
  # check if spatial or not
  # same as source("defineClass_tmp.r") - think if this is the place?
  # check if class(nummar) exists.
 
  

  um <- new("nummar", uncertain = uncertain, distribution = distribution, paramdist = paramdist, meanfun = meanfun)

  return(um)
}

# Example
a <- defnummar(uncertain = TRUE, distribution = "norm", paramdist = c(2, 0.1))
a
