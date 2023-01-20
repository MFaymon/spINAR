#' @title Simulate, estimate and bootstrap integer autoregressive (INAR) models
#'
#' @description Flexible simulation of INAR data using a general pmf to define
#' the innovations' distribution, semiparametric and parametric estimation of
#' INAR models (including a small sample refinement for the semiparametric
#' setting) and different procedures to appropriately bootstrap INAR data.
#'
#' @references
#' Faymonville, M., Jentsch, C., Weiß, C.H. und Aleksandrov, B. (2022).
#' Semiparametric Estimation of INAR Models using Roughness Penalization.
#' Statistical Methods & Applications.
#'
#' Jentsch, C. and Weiß, C. H. (2017), “Bootstrapping INAR Models”.
#' Bernoulli 25(3), pp. 2359-2408.
#'
#' Drost, F., Van den Akker, R. and Werker, B. (2009), “Efficient estimation of
#' auto-regression parameters and inovation distributions for semiparametric
#' integer-valued AR(p) models”. Journal of the Royal Statistical Society.
#' Series B 71(2), pp. 467-485.
#'
#' @import stats
#' @import checkmate
"_PACKAGE"
