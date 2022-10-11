
# spINAR

<!-- badges: start -->
<!-- badges: end -->

The goal of spINAR is to:

  1) Simulate INAR(p) data with arbitrary innovation distributions (parametric family or probability mass function).

  2) Semiparametric estimation of INAR(p) model.

  3) Semiparametric INAR Boostrap.

  4) Fully parametric estimation of INAR(p) model.

  5) Penalized (semiparametric) estimation of INAR(p) model.
  


## Installation

For installation of the development version use [devtools](https://cran.r-project.org/package=devtools):

``` r
devtools::install_github("MFaymon/spINAR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(spINAR)
dat <- c(2,3,1,1,1,1,1,1,3)
p <- 1
spinar(dat, p)
```

