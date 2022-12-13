
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

## Structure

![](https://github.com/MFaymon/spINAR/blob/main/img_readme/cheat_sheet_spINAR.png)

## Examples

### Dataset poisson innovations with p = 1

```r
n <- 500 # sample size
m <- 100 # additional observations to ensure stationarity
alpha <- 0.5 # true INAR(1) coefficient
lambda <- 0.8 # true parameter of the poisson innovation distribution

pinar1 <- function(n, alpha, lambda) {
  err <- rpois(n, lambda)
  x <- numeric(n)
  x[1] <- err[1]
  for (i in 2:n) {
    x[i] <- rbinom(1, x[i - 1], alpha) + err[i]
  }
  return(x)
}
sim1 <- pinar1(n+m, alpha, lambda)[-(1:m)] # remove first m observations to have stationarity
```

### Dataset poisson innovations with p = 2

```r
n <- 500 
m <- 100
alpha1 <- 0.3
alpha2 <- 0.5
lambda <- 0.8

pinar2 <- function(n, alpha1, alpha2, lambda) {
  err <- rpois(n, lambda)
  x <- numeric(n)
  x[1] <- err[1]
  x[2] <- x[1] + err[2]
  for (i in 3:n) {
    x[i] <-
      rbinom(1, x[i - 1], alpha1) + rbinom(1, x[i - 2], alpha2) + err[i]
  }
  return(x)
}
sim2 <- pinar2(n, alpha1, alpha2, lambda)[-(1:m)]
```

### Example 1: Semiparametric Estimation of INAR Models

In this example, we simulate INAR(1) data with poisson distributed innovations.

```r
params_est <- spINAR::spinar_est(x= sim1, p=1)

# estimation of INAR(1) coefficient alpha1
alpha_est <- params_est[1]  # close to alpha = 0.5

# estimation of pmf/innovation distribution (pmf0, pmf1, ...)
pmf_est <- params_est[-1] 
```

Simulating 100 iterations for each sample size of poisson innovations with alpha 0.5 and lambda 0.8 we got the following average values for alpha and the probability mass function. 


![](https://github.com/MFaymon/spINAR/blob/main/img_readme/spinar_est_alpha.jpg) 
![](https://github.com/MFaymon/spINAR/blob/main/img_readme/spinar_est_pmf.jpg)

### Example 2: Parametric estimation of INAR models 
