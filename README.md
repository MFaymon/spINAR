
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

## Examples

``` r
# Install library
library(package1)
```

### Example 1

In this example, we simulate INAR(1) data with poisson distributed innovations.

``` r
n <- 500 # sample size
m <- 100 # additional observations to ensure stationarity
alpha <- 0.5 # true INAR(1) coefficient
lambda <- 1 # true parameter of the poisson innovation distribution

pinar1 <- function(n, alpha, lambda){
  err <- rpois(n, lambda)
  x <- numeric(n)
  #initialization x_0 = 0
  x[1] <- err[1]
  for(i in 2:n){
    x[i] <- rbinom(1, x[i-1], alpha) + err[i]
  }
  return(x)
}

sim <- pinar1(n+m, alpha, lambda)
sim <- sim[-(1:m)] # remove first m observations
```

Now, we estime the parameters:

``` r
params_est <- package1::spinar(sim,1)
alpha_est <- params_est[1] # estimation of INAR(1) coefficient alpha1
pmf_est <- params_est[-1]) # estimation of pmf/innovation distribution (pmf0, pmf1, ...)
```
### Example 2

This is a basic example which shows you how to solve a common problem for INAR(2)

``` r
params_est <- package1::spinar(c(3,2,1,1,1,2,2,3),2)
alpha_est <- params_est[1:2] # estimation of INAR(2) coefficient (alpha1, alpha2)
pmf_est <- params_est[-(1:2)] # estimation of pmf/innovation distribution (pmf0, pmf1, pmf2, pmf3)
```

