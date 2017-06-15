# spup
R package: spup - Spatial Uncertainty Propagation Analysis

Description
===========

Uncertainty propagation analysis in spatial environmental modelling following methodology described in Heuvelink et al. (2007) and Brown and Heuvelink (2007). The package provides functions for examining the uncertainty propagation starting from input data and model parameters, via the environmental model onto model outputs. The functions include uncertainty model specification, stochastic simulation and propagation of uncertainty using Monte Carlo (MC) techniques. Uncertain variables are described by probability distributions. Both numerical and categorical data types are handled. Spatial auto-correlation within an attribute and cross-correlation between attributes is accommodated for. The MC realizations may be used as input to the environmental models called from R, or externally.

Installation
------------

R package _spup_ is available on CRAN and can be installed in R as:

``` r
install.packages("spup")
```

The development version from GitHub can be install via:

``` r
library(devtools)
install_github("ksawicka/spup")
```

References
----------

Brown, J. D. and G. B. M. Heuvelink (2007). "The Data Uncertainty Engine (DUE): A software tool for assessing and simulating uncertain environmental variables." Computers & Geosciences 33(2): 172-190.

Heuvelink, G. B. M., et al. (2007). "A probabilistic framework for representing and simulating uncertain environmental variables." International Journal of Geographical Information Science 21(5): 497-513.
