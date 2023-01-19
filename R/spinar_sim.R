#' @title Simulation of (semi)parametric integer autoregressive (INAR) models
#'
#' @description Generating INAR(p) observations, where \code{p} \eqn{\in \{1,2\}}. It allows for general pmfs
#' which can be generated parametrically or "manually" (semiparametrically).
#'
#' @param n [\code{integer(1)}]\cr
#' number of observations.
#' @param p [\code{integer(1)}]\cr
#' lag of the INAR(\code{p}) model, where \code{p} \eqn{\in \{1,2\}}.
#' @param alpha [\code{integer(p)}]\cr
#' vector of INAR coefficients \eqn{\code{alpha}_1,...,\code{alpha}_p}.
#' @param pmf [\code{numeric}]\cr
#' vector of probability mass function \eqn{\code{pmf}_0,..., \code{pmf}_k} where \eqn{\code{pmf}_i} represents the probability of
#' an innovation being equal to \eqn{i}.
#' @param prerun [\code{integer(1)}]\cr
#' number of observations which are generated additionally and then omitted (to ensure stationarity).
#'
#' @return Vector with \eqn{n} INAR(\code{p}) observations.
#'
#' @examples # generate (semiparametrically) 100 INAR(1) observations with alpha_1 = 0.5 and a manually set pmf
#' spinar_sim(n = 100, p = 1, alpha = 0.5, pmf = c(0.3, 0.3, 0.2, 0.1, 0.1))
#'
#' @examples # generate 100 obervations of an INAR(2) model with alpha_1 = 0.2, alpha_2 = 0.3 and Poi(1)-innovations
#' spinar_sim(n = 100, p = 2, alpha = c(0.2, 0.3), pmf = dpois(0:20,1))
#'
#' @export spinar_sim
spinar_sim <- function(n, p, alpha, pmf, prerun = 500) {
  checkmate::assert_integerish(p, lower = 1, upper = 2)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = p)
  checkmate::assert_numeric(pmf, lower = 0, upper = 1, min.len = p+1)
  checkmate::assert_integerish(n, lower = 0)
  checkmate::assert_integerish(prerun, min = 0)
  if (round(sum(pmf), 6) != 1) {
    warning("Sum of pmf entries has been standardized to 1.")
  }
  pmf <- pmf / sum(pmf)
  err <-
    sample(0:(length(pmf) - 1),
           n + prerun,
           replace = TRUE,
           prob = pmf)
  x <- numeric(n + prerun)
  x[1] <- err[1]
  if (p == 1) {
    for (i in 2:(n + prerun)) {
      x[i] <- rbinom(1, x[i - 1], alpha) + err[i]
    }
  }
  else {
    alpha1 <- alpha[1]
    alpha2 <- alpha[2]
    x[2] <- x[1] + err[2]
    for (i in 3:(n + prerun)) {
      x[i] <-
        rbinom(1, x[i - 1], alpha1) + rbinom(1, x[i - 2], alpha2) + err[i]
    }
  }
  return(x[-seq_len(prerun)])
}

