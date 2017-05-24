# spup
R package: spup - Spatial Uncertainty Propagation Analysis

Description
===========

Uncertainty propagation analysis in spatial environmental modelling following methodology described in Heuvelink et al. (2017) <doi:10.1080/13658810601063951> and Brown and Heuvelink (2007) <doi:10.1016/j.cageo.2006.06.015>. The package provides functions for examining the uncertainty propagation starting from input data and model parameters, via the environmental model onto model outputs. The functions include uncertainty model specification, stochastic simulation and propagation of uncertainty using Monte Carlo (MC) techniques. Uncertain variables are described by probability distributions. Both numerical and categorical data types are handled. Spatial auto-correlation within an attribute and cross-correlation between attributes is accommodated for. The MC realizations may be used as input to the environmental models called from R, or externally.

Installation
------------

R package _spup_ is available on CRAN and can be installed in R as:

``` r
install.packages("spup")
```

