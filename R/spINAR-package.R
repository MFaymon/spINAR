#' @title (Semi)parametric estimation and bootstrapping of INAR models
#'
#' @description Semiparametric and parametric estimation of INAR models
#' including a finite sample refinement for the semiparametric setting, different
#' procedures to bootstrap INAR data and flexible simulation of INAR data.
#'
#' @section Semiparametric INAR Model:
#' The package provides a flexible simulation of INAR data by inserting a user-defined
#' pmf argument in the \code{\link{spinar_sim}} function. Using \code{\link{spinar_est}},
#' it allows for semiparametric estimation of the INAR model along Drost et al. (2009)
#' and additionally, it includes a small sample refinement \code{\link{spinar_penal}}
#' (Faymonville et al., 2022) together with a validation of the upcoming penalization
#' parameters (\code{\link{spinar_penal_val}}). Furthermore, it contains a semiparametric
#' INAR bootstrap procedure implemented in \code{\link{spinar_boot}} (Jentsch and Weiß, 2017).
#'
#' @section Parametric INAR Model:
#' In addition to the semiparametric model, the package also allows for parametric simulation
#' (\code{\link{spinar_sim}}), parametric estimation (\code{\link{spinar_est_param}}) and
#' parametric bootstrapping (\code{\link{spinar_boot}}) of INAR data.
#'
#' @references
#' Faymonville, M., Riffo, J., Rieger, J. and Jentsch, C. (2024).
#' "spINAR: An R Package for Semiparametric and Parametric Estimation and
#' Bootstrapping of Integer-Valued Autoregressive (INAR) Models".
#' Journal of Open Source Software 9(97), pp. 5386. \doi{10.21105/joss.05386}.
#'
#' Faymonville, M., Jentsch, C., Weiß, C.H. and Aleksandrov, B. (2022).
#' "Semiparametric Estimation of INAR Models using Roughness Penalization".
#' Statistical Methods & Applications. \doi{10.1007/s10260-022-00655-0}.
#'
#' Jentsch, C. and Weiß, C. H. (2017), "Bootstrapping INAR Models".
#' Bernoulli 25(3), pp. 2359--2408. \doi{10.3150/18-BEJ1057}.
#'
#' Drost, F., Van den Akker, R. and Werker, B. (2009), "Efficient estimation of
#' auto-regression parameters and innovation distributions for semiparametric
#' integer-valued AR(p) models". Journal of the Royal Statistical Society.
#' Series B 71(2), pp. 467--485. \doi{10.1111/j.1467-9868.2008.00687.x}.
#'
#' @import stats
#' @import checkmate
#' @import progress
"_PACKAGE"
