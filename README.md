
# spINAR

<!-- badges: start -->
<!-- badges: end -->

The goal of spINAR is to :

  1) Simulate INAR(p) data with arbitrary innovation distributions (parametric family or probability mass function).

  2) Semiparametric estimation of INAR(p) model.

  3) Semiparametric INAR Boostrap.

  4) Fully parametric estimation of INAR(p) model.

  5) Penalized (semiparametric) estimation of INAR(p) model.
  


## Installation

You can install the development version of package1 like so:

``` r
# install.package("spINAR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(spINAR)
## basic example code
dat <- c(2,3,1,1,1,1,1,1,3)
p <- 1
spINAR::spinar(dat, p)
```

