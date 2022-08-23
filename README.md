
# package1

<!-- badges: start -->
<!-- badges: end -->

The goal of package1 is to :

  1) Simulate INAR(p) data with arbitrary innovation distributions (parametric family or probability mass function).

  2) Semiparametric estimation of INAR(p) model.

  3) Semiparametric INAR Boostrap.

  4) Fully parametric estimation of INAR(p) model.

  5) Penalized (semiparametric) estimation of INAR(p) model.
  


## Installation

You can install the development version of package1 like so:

``` r
# install.package("package1")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(package1)
## basic example code
dat <- c(2,3,1,1,1,1,1,1,3)
p <- 1
upper <- 3
par <- c(0.1,0.8, 0.7, 0.6, 0.4)
spinar(dat, p, upper, par)
```

