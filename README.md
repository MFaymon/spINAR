
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

### Example 1: Semiparametric Estimation of INAR Models

In this example, we simulate INAR(1) data with poisson distributed innovations.

```r
pinar1 <- function(n, alpha, lambda) {
  err <- rpois(n, lambda)
  x <- numeric(n)
  x[1] <- err[1]
  for (i in 2:n) {
    x[i] <- rbinom(1, x[i - 1], alpha) + err[i]
  }
  return(x)
}

SpinarEst <- function(n, k){
  sim <- replicate(k, spINAR::spinar_est(x= pinar1(n, alpha=0.8, lambda= 0.8), p=1))
  sum = 0
  for (i in c(1:k)){
    sum = sum + sim[[i]][[1]]
  }
  return(sum/k)
}
```

Simulating 100 iterations for each sample size of poisson innovations with alpha 0.8 and lambda 0.8 we got the following average values for alpha.

![](https://github.com/MFaymon/spINAR/blob/main/img_readme/spinar_est_example_convergence_alpha.png) 

### Example 2: Parametric estimation of INAR models 

In this example, we observe a convergence to the asymptotic probability mass function (pmf) by increasing the sample size (n) and the number of iterations (k) in the `spinar_sim` function.

```r
SimSpinar <- function(n,prerun, k){
  sim <- replicate(k, spINAR::spinar_sim(n, p=1, alpha=0.8, pmf=dpois(0:20, lambda), prerun))
  simprobs <- table(sim)/(k*n)
  return(simprobs = simprobs)
}

asymptotic <- SimSpinar(n=100000, prerun = 5000, k = 1000)
```

![](https://github.com/MFaymon/spINAR/blob/main/img_readme/spinar_sim_example_convergence.png)

### Example 3: Semiparametric INAR Boostrap

```r
dat <- spINAR::spinar_sim(n=1000, p = 1, alpha = 0.3, pmf = dpois(0:10,1.5))
x1 <- spINAR::spinar_boot(x = dat, p = 1, B = 100)
y1 <- spINAR::spinar_boot(x = dat, p = 1, B = 250)
z1 <- spINAR::spinar_boot(x = dat, p = 1, B = 500)
w1 <- spINAR::spinar_boot(x = dat, p = 1, B = 750)
```

![](https://github.com/MFaymon/spINAR/blob/main/img_readme/pmf_convergence_boostrap.png)
