# spINAR
[![DOI](https://joss.theoj.org/papers/10.21105/joss.05386/status.svg)](https://doi.org/10.21105/joss.05386)
[![CRAN](https://www.r-pkg.org/badges/version/spINAR)](https://cran.r-project.org/package=spINAR)
[![R build status](https://github.com/MFaymon/spINAR/workflows/R-CMD-check/badge.svg)](https://github.com/MFaymon/spINAR/actions)
[![codecov](https://codecov.io/gh/MFaymon/spINAR/branch/main/graph/badge.svg?token=U5KPFSY3XN)](https://app.codecov.io/gh/MFaymon/spINAR)
[![DOI](https://zenodo.org/badge/520425653.svg)](https://zenodo.org/badge/latestdoi/520425653)

Semiparametric and Parametric Estimation and Bootstrapping of Integer-Valued Autoregressive (INAR) Models.

The package provides flexible simulation of INAR data using a general pmf to define the innovations' distribution. It allows for semiparametric and parametric estimation of INAR models and includes a small sample refinement for the semiparametric setting. Additionally, it provides different procedures to appropriately bootstrap INAR data.

## Citation
Please cite the [JOSS](https://doi.org/10.21105/joss.05386) paper using the BibTeX entry
```
@article{faymonville2024spinar,
  doi = {10.21105/joss.05386},
  url = {https://doi.org/10.21105/joss.05386},
  year = {2024},
  publisher = {The Open Journal},
  volume = {9},
  number = {97},
  pages = {5386},
  author = {Maxime Faymonville and Javiera Riffo and Jonas Rieger and Carsten Jentsch},
  title = {spINAR: An R Package for Semiparametric and Parametric Estimation and Bootstrapping of Integer-Valued Autoregressive (INAR) Models},
  journal = {Journal of Open Source Software}
} 

```
which is also obtained by the call ``citation("spINAR")``.

## References (related to the methodology)
* Faymonville, M., Jentsch, C., Weiß, C.H. and Aleksandrov, B. (2022). "Semiparametric Estimation of INAR Models using Roughness Penalization". Statistical Methods & Applications. [DOI](https://doi.org/10.1007/s10260-022-00655-0)
* Jentsch, C. and Weiß, C.H. (2017), “Bootstrapping INAR Models”. Bernoulli 25(3), pp. 2359-2408. [DOI](https://doi.org/10.3150/18-BEJ1057)
* Drost, F., Van den Akker, R. and Werker, B. (2009), “Efficient estimation of auto-regression parameters and inovation distributions for semiparametric integer-valued AR(p) models”. Journal of the Royal Statistical Society. Series B 71(2), pp. 467-485. [DOI](https://doi.org/10.1111/j.1467-9868.2008.00687.x)

## Contribution
This R package is licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).
For bug reports (lack of documentation, misleading or wrong documentation, unexpected behaviour, ...) and feature requests please use the [issue tracker](https://github.com/MFaymon/spINAR/issues).
Pull requests are welcome and will be included at the discretion of the author.

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

## Application
The file vignette.md provides reproduced results from the literature for each provided functionality of the spINAR package.

## Outlook

A possible extension of the spINAR package is to not only cover INAR models but also the extension to GINAR (generalized INAR) models, see [Latour (1997)](https://doi.org/10.1017/s0001867800027865).  This model class does not only cover the binomial thinning but also allows for other thinning operations, e.g. thinning using geometrically distributed random variables. 
