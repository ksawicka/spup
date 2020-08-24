\name{NEWS}

\title{News for Package \pkg{'spup'}}

\section{Changes in version 1.3-2 (2020-04-30)}{
  \itemize{
    \item corrected checks on class(matrix(...)) following R Update that matrix objects now also inherit from class "array", namely, e.g. class(diag(1)) is c("matrix", "array") which invalidates code assuming that length(class(obj)) == 1
  }
}

\section{Changes in version 1.3-1 (2018-06-25)}{
  \itemize{
    \item fixed a bug related to derivation of cross-correlation matrix for multivariate uncertainty propagation analysis
    \item changed mainteiner email address
  }
}

\section{Changes in version 1.2-1 (2018-03-14)}{
  \itemize{
    \item added automated tests
  }
}

\section{Changes in version 0.1-1 (2017-07-24)}{
  \itemize{
    \item changed mainteiner email address 
  }
}

\section{Changes in version 0.1-0 (2017-04-13)}{
  \itemize{
    \item first release
  }
}
