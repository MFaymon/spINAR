#' @title Simulation of (semi)parametric integer autoregressive models
#' @description Function to generate INAR(p), where p can be 1 or 2 and we allow for general pmf's which can be generated parametrically or "manually" (semiparametric)
#' @param n : number of INAR(p) values generated
#' @param p : parameter of the INAR(p)
#' @param alpha : alpha_1,...,alpha_p
#' @param pmf : probability mass function g_0,...,g_m
#' @param prerun : default parameter equal 500. The function generate, n + prerun observations and then cut the first prerun observations.
#'
#' @return vector with n simulations
#' @export
#'
#' @examples spinar_sim(1000, 1, 0.5, dpois(0:20,1), prerun = 500)
#' @examples spinar_sim(1000, 1, 0.5, c(0.1,0.5,0.4), prerun = 500)
#' @examples spinar_sim(1000, 1, 0.5, c(0.1,0.5,0.2), prerun = 500)
#' @examples spinar_sim(1000, 2, c(0.2, 0.3), dpois(0:20,1), prerun = 500)
#' @examples spinar_sim(1000, 2, c(0.2, 0.3), c(0.1,0.5,0.4), prerun = 500)
#' @examples spinar_sim(1000, 2, c(0.2, 0.3), c(0.1,0.5,0.2), prerun = 500)


spinar_sim <- function(n, p, alpha, pmf, prerun = 500) {
  checkmate::assert_integerish(p, lower = 1, upper = 2, len = length(alpha))
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = p)
  checkmate::assert_numeric(pmf, lower = 0, upper = 1, min.len = p+1) # this is true?
  checkmate::assert_integerish(n, lower = 0) # lower value?
  if (sum(pmf) != 1) {
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
  # case p = 1
  if (p == 1) {
    for (i in 2:(n + prerun)) {
      x[i] <- rbinom(1, x[i - 1], alpha) + err[i]
    }
  }
  # case p = 2
  else {
    x[2] <- x[1] + err[2]
    for (i in 3:(n + prerun)) {
      x[i] <-
        rbinom(1, x[i - 1], alpha1) + rbinom(1, x[i - 2], alpha2) + err[i]
    }
  }
  return(x[-seq_len(prerun)])
}

