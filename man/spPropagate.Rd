\name{spPropagate}
\alias{spPropagate}
\title{Executing selected model runs with uncertain inputs}

% Description section
\description{
Function that allows user to run selected model number of times using MC sample.
}

% Usage section
\usage{
runmodel(model, inputsample, numruns, ...)
}

% Arguments section
\arguments{
  \item{model}{ user specific function. }
  \item{inputsample}{ output of realizeMC. } % font of realizeMC according to the variable font
  \item{numruns}{ number of model runs. }
}

% Details section
\details{
 \strong{model} has to be a user specific function that uses inputsample as an argument.
}

% Value section
\value{
A list with num model runs. % font of num as a function argument
}

% Author(s) section
\author{ Kasia Sawicka, Gerard Heuvelink}

% References section
\references{}

% Examples section
\examples{
myexamples()
}
