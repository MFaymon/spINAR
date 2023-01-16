# spINAR
[![R build status](https://github.com/JonasRieger/ldaPrototype/workflows/R-CMD-check/badge.svg)](https://github.com/JonasRieger/ldaPrototype/actions)

Simulate, estimate and bootstrap integer autoregressive (INAR) models.

The package provides flexible simulation of INAR data using a general pmf to define the innovations' distribution. It allows for semiparametric and parametric estimation of INAR models and includes a small sample refinement for the semiparametric setting. Additionally, it provides different procedures to appropriately bootstrap INAR data.

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

### Example 4: Fully parametric estimation of INAR(p) model
In this example, we generate data with geometric distribution over 500 iterations comparing the Maximum Likelihood and Method of Moments for different values of the sample size.

```r
geominar1 <- function(n, alpha, pr) {
  err <- rgeom(n, pr)
  x <- numeric(n)
  x[1] <- err[1]
  for (i in 2:n) {
    x[i] <- rbinom(1, x[i - 1], alpha) + err[i]
  }
  return(x)
}

SpinarEstParam <- function(n, k, method){
  sim <- replicate(k, spINAR::spinar_est_param(x = geominar1(n,0.3, 0.5), p=1, type = method, distr = "geo"))
  alpha1 = 0
  prob = 0
  for (i in c(1:k)){
    alpha1 = alpha1 + sim[,i][[1]]
    prob = prob + sim[,i][[2]]
  }
  return(list(alpha1/k, prob/k))
}
```

![](https://github.com/MFaymon/spINAR/blob/main/img_readme/example_spinar_est_param_geom.png)
