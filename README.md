
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

### Example 1: Parametric estimation of INAR models

In this example, we simulate INAR(1) data with poisson distributed innovations.

```r
params_est <- spINAR::spinar_est(x= sim1, p=1)

# estimation of INAR(1) coefficient alpha1
alpha_est <- params_est[1]  # close to alpha = 0.5

# estimation of pmf/innovation distribution (pmf0, pmf1, ...)
pmf_est <- params_est[-1] 
```

Simulating 100 iterations for each sample size of poisson innovations with alpha 0.5 and lambda 0.8 we got the following average values for alpha and the probability mass function. 

```r
alpha_list <- c()
pmf_list <- c()
N <- seq(from = 50, to = 1000, by = 50)
M <- 100
for (n in N) {
  i = 1
  alpha_sum = 0
  pmf_join = c()
  pmf_mean <- c()
  print(n)
  while (i <= M) {
    data <- pinar1(n, 0.5, 0.8)
    params_est <- spINAR::spinar_est(x = data, p = 1)
    alpha_sum = alpha_sum + params_est[1]
    pmf <- params_est[-1][1:8]
    pmf[is.na(pmf)] <- 0
    pmf_join <- append(pmf_join, pmf)
    i = i + 1
  }
  alpha_mean = alpha_sum / M
  alpha_list <- append(alpha_list, alpha_mean)
  aux <- as.data.frame(split(pmf_join,
                             cut(seq_along(pmf_join),
                                 M,
                                 labels = FALSE)))
  for (s in seq(1, 8)) {
    pmf_mean = append(pmf_mean, sum(aux[s, 1:M]) / M)
  }
  pmf_list <- append(pmf_list, pmf_mean)
}
```

![](https://github.com/MFaymon/spINAR/blob/main/img_readme/spinar_est_alpha.jpg) 
![](https://github.com/MFaymon/spINAR/blob/main/img_readme/spinar_est_pmf.jpg)


