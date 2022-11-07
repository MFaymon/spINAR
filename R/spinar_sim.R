#' @title Simulation of (semi)parametric integer autoregressive models
#'
#' @description Function to generate INAR(p), where p in {1,2} and we allow for general pmf's which can be generated parametrically or "manually" (semiparametric)
#'
#' @param n [\code{integer}(1)]\cr
#' total of values generated for the INAR(p).
#' @param p [\code{integer}(1)]\cr
#' lag of the INAR(p) where \code{p in \{1,2\}}.
#' @param alpha [\code{integer(p)}]\cr
#' alpha_1,...,alpha_p
#' @param pmf [\code{numeric}]\cr
#' probability mass function pmf0,..., pmfk where pmfi represent the pmf for the value i.
#' @param prerun  [\code{integer(1)}]\cr
#' default parameter equal 500, the function `spinar_sim` uses `n + prerun` observations and then cut the first prerun observations.
#'
#' @return sim  [\code{integer(n)}]\cr
#' vector with n simulations for the INAR(p) generated.
#' @export
#'
#' @examples spinar_sim(1000, 1, 0.5, dpois(0:20,1), prerun = 500)
#' @examples spinar_sim(1000, 1, 0.5, c(0.1,0.5,0.4), prerun = 500)
#' @examples spinar_sim(1000, 1, 0.5, c(0.1,0.5,0.2), prerun = 500)
#' @examples spinar_sim(1000, 2, c(0.2, 0.3), dpois(0:20,1), prerun = 500)
#' @examples spinar_sim(1000, 2, c(0.2, 0.3), c(0.1,0.5,0.4), prerun = 500)
#' @examples spinar_sim(1000, 2, c(0.2, 0.3), c(0.1,0.5,0.2), prerun = 500)


spinar_sim <- function(n, p, alpha, pmf, prerun = 500) {
  checkmate::assert_integerish(p, lower = 1, upper = 2)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = p)
  checkmate::assert_numeric(pmf, lower = 0, upper = 1, min.len = p+1) # is this true?
  checkmate::assert_integerish(n, lower = 0) # lower value?
  checkmate::assert_integerish(prerun, min = 0)  # include a max(n)?
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

