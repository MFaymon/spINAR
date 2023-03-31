# spINAR
[![status](https://joss.theoj.org/papers/6fcfcc77635fdd18153b35d5986fe2aa/status.svg)](https://github.com/openjournals/joss-reviews/issues/5152)
[![CRAN](https://www.r-pkg.org/badges/version/spINAR)](https://cran.r-project.org/package=spINAR)
[![R build status](https://github.com/MFaymon/spINAR/workflows/R-CMD-check/badge.svg)](https://github.com/MFaymon/spINAR/actions)
[![codecov](https://codecov.io/gh/MFaymon/spINAR/branch/main/graph/badge.svg?token=U5KPFSY3XN)](https://app.codecov.io/gh/MFaymon/spINAR)
[![DOI](https://zenodo.org/badge/520425653.svg)](https://zenodo.org/badge/latestdoi/520425653)

Semiparametric and Parametric Estimation and Bootstrapping of Integer-Valued Autoregressive (INAR) Models.

The package provides flexible simulation of INAR data using a general pmf to define the innovations' distribution. It allows for semiparametric and parametric estimation of INAR models and includes a small sample refinement for the semiparametric setting. Additionally, it provides different procedures to appropriately bootstrap INAR data.

## References (related to the methodology)

* Faymonville, M., Jentsch, C., Weiß, C.H. und Aleksandrov, B. (2022). Semiparametric Estimation of INAR Models using Roughness Penalization. Statistical Methods & Applications. [DOI](https://doi.org/10.1007/s10260-022-00655-0)
* Jentsch, C. and Weiß, C.H. (2017), “Bootstrapping INAR Models”. Bernoulli 25(3), pp. 2359-2408. [DOI](https://doi.org/10.3150/18-BEJ1057)
* Drost, F., Van den Akker, R. and Werker, B. (2009), “Efficient estimation of auto-regression parameters and inovation distributions for semiparametric integer-valued AR(p) models”. Journal of the Royal Statistical Society. Series B 71(2), pp. 467-485. [DOI](https://doi.org/10.1111/j.1467-9868.2008.00687.x)

## Installation

For installation of the development version use [devtools](https://cran.r-project.org/package=devtools):

``` r
devtools::install_github("MFaymon/spINAR")
```

## Structure
![](https://github.com/MFaymon/spINAR/blob/main/img_readme/cheat_sheet_spINAR.png)

## Example

```r
library(spINAR)
```

We simulate two datasets. The first consists of n = 100 observations resulting from an INAR(1) model with coefficient alpha = 0.5 and Poi(1) distributed innovations. The second consists of n = 100 observations from an INAR(2) model with coefficients alpha_1 = 0.3, alpha_2 = 0.2 and a pmf equal to (0.3, 0.3, 0.2, 0.1, 0.1).

```r
set.seed(1234)

dat1 <- spinar_sim(100, 1, alpha = 0.5, pmf = dpois(0:20,1))
dat2 <- spinar_sim(100, 2, alpha = c(0.3, 0.2), pmf = c(0.3, 0.3, 0.2, 0.1, 0.1))
```

We estimate an INAR(1) model on the first dataset.

```r
#semiparametrically
spinar_est(dat1, 1)

#parametrically (moment estimation, true Poisson assumption)
spinar_est_param(dat1, 1, "mom", "poi")
```

We estimate an INAR(2) model on the second dataset.

```r
#semiparametrically
spinar_est(dat2, 2)
```

For small samples, it can be beneficial to apply a penalized version of the semiparametric estimation. For illustration, we restrict ourselves to the first 50 observations of the first dataset and apply semiparametric, parametric and penalized semiparametric estimation. We choose a small L2 penalization as this showed to be most beneficial in the simulation study in Faymonville et al. (2022) (see references). Alternatively, one could also use the spinar_penal_val function which validates the two penalization parameters.

```r
dat1_50 <- dat1[1:50]
spinar_est(dat1_50, 1)
spinar_est_param(dat1_50, 1, "mom", "poi")
spinar_penal(dat1, 1, penal1 = 0, penal2 = 0.1)
```

Finally, we bootstrap INAR(1) data on the first data set. We perform a semiparametric and a parametric INAR bootstrap (moment estimation, true Poisson assumption). 

```r
spinar_boot(dat1, 1, 500, setting = "sp")
spinar_boot(dat1, 1, 500, setting = "p", type = "mom", distr = "poi")
```

