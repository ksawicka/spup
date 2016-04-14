\name{crm2vgm}
\alias{crm2vgm}
\title{Converting a spatial correlogram model to a variogram model}

% Description section
\description{
Used internally in genSample() in case of sampling by unconditional gaussian simulation.
}

% Usage section
\usage{
crm2vgm(crm, ...)
}

% Arguments section
\arguments{
  \item{crm}{ Spatial correlogram model, output of makecrm(). }
  \item{...}{ Parameters that can be passed to vgm(). }
}

% Details section
\details{
To assure equalfinality the sill parameter for spatially correlated random residuals is fixed and standardized to 1.
}

% Value section
\value{
An object of a class "variogramModel" extending data.frame.
}

% Author(s) section
\author{ Kasia Sawicka, Gerard Heuvelink}

% References section
\references{}

% Examples section
\examples{
mycormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
myvgm <- crm2vgm(mycormodel)
myvgm
}
