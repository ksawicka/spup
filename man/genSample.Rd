\name{genSample}
\alias{genSample}
\title{Generating Monte Carlo sample from an uncertain object}

% Description section
\description{
  Function that runs Monte Carlo simulations depending on the type of uncertain object. Facilitates unconditional gausian simularion of errors for spatially auto-correlated residulas, and random sampling, stratified sampling and latin hypercube sampling if no spatial auto-correlation is included.
}

% Usage section
\usage{
  genSample(n, uncert.object, samplemethod, p = 0, ...)
}

% Arguments section
\arguments{
  \item{n}{ Integer. Number of Monte Carlo realizations. }
  \item{uncert.object}{ Uncertain object of one of the classes: "NumMarSpatial", "NumMarNonSpatial", "categorical", "joint?". } % fill in when decided what the other classes are.
  \item{samplemethod}{ "ugs" for spatially correlated errors, "randomSampling", "stratifiedSampling", "lhs" if no spatial correlation of erros is considered. }
  \item{p}{ A vector of quantiles. Optional. Only required if sample method is "stratifiedSampling" or "lhs". }
  \item{...}{ Additional parameters that may be passed, e.g. to the predict in the "ugs" method. }
}

% Details section
\details{
  The function simulate realizations of residuals and calculates the simulations of an uncertan object according to X = m + sd * \eqn{\epsilon}.

  Returned map of sd is based on sd calculated per grid cell for all simulations of X.

  \strong{"ugs"}{ Unconditional gaussian simulation of spatially auto-correlated errors. }

  \strong{"stratifiedSampling"}{ Number of samples (n) must be dividable by the number of quantiles to assure each quantile is evenly represented.  }

  \strong{"lhs"}{ Sampling method for at least two uncertain inputs. The uncertain.object is then a list of two or more. It uses startified sampling method to generate the inputs for the latin hypercude algorithm, hence the p is restricted as above. }

}


% Value section
\value{
  An object of a class "SpatialMCSample" or "NonSpatialMCSample" depending on the class of uncert.object. This is a list with the following items:
  \item{X}{ Simulated realizations of the spatial uncertain object. }
  \item{map.of.sd}{ A map of sd is based on sd calculated per grid cell for all simulations of X. }
  \item{sampling.method}{ A string with a selected sampling method. }
  \item{crm}{ Spatial correlogram model. The slot only exists if spatial auto-correlation of errors is taken into account. }
}

% Author(s) section
\author{ Kasia Sawicka, Stefan van Dam, Gerard Heuvelink}

% References section
\references{}

% Examples section
\examples{
  ## Spatial objects examples
  # "ugs" example
  data(geul)
  geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
  geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
  geul.sim <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "ugs", nmax = 20)

  # "randomSampling" example
  data(geul)
  geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, mask = geul.mask)
  geul.sim <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "randomSampling")

  # "startifiedSampling" example
  data(geul)
  geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, mask = geul.mask)
  geul.sim <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "stratifiedSampling", p = 0:5/5)

  # "lhs" example
  data(geul)
  geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, mask = geul.mask)
  geul.sc <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(-2.958, 1.294))
  list.of.geul <- list(geul.pb = geul.pb, geul.sc = geul.sc)
  geul.sim <- genSample(n = 5, uncert.object = list.of.geul, samplemethod = "lhs", p = 0:5/5)

  ## Skalar example
  geul.sc <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(-2.958, 1.294))
  geul.sim <- genSample(n = 5, uncert.object = geul.sc, samplemethod = "randomSampling")

}
