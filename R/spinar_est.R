#' @title Semiparametric Estimation of INAR Models
#'
#' @description
#' Performs a semiparametric estimation of the autoregressive parameters and the innovation distribution for INAR models of order \code{p},
#'  \code{p in {1,2}}, the estimation is conducted by maximizing the conditional likelihood of the model.
#'
#' @param x [\code{integer}]\cr
#' vector of integer values corresponding to the data
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \code{p in \{1,2\}}
#'
#' @return estimated parameters \code{(alpha_1, ..., alpha_p, pmf[0], pmf[1], ...)},
#' where \code{(alpha_1, ..., alpha_p)} are the estimated autoregressive coefficients
#' and \code{(pmf[0], pmf[1], ...)} are the estimated entries of the probability mass function of the innovation distribution, 
#' where \code{pmf[i]} denotes the probability of observing value i
#'
#' @examples
#' ## Parameter estimation for p = 1 and x as a vector of values.
#' dat <- spinar_sim(100, 1, 0.5, dpois(0:10,1))
#' spinar_est(dat, 1)
#' ### returns (alpha_1, pmf[0], pmf[1],...)
#' ## Parameter estimation for p = 2 and data generated from a poisson distribution with parameter 0.8.
#' dat <- spinar_sim(100, 2, 0.2, 0.3, dpois(0:10,1))
#' spinar_est(dat, 2)
#' ### returns (alpha_1, alpha_2, pmf[0], pmf[1], ...)
#' @export

spinar_est <- function(x, p) {
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  checkmate::assert_integerish(x, min.len = p+1)
  xmax <- max(x)
  if(p==1){
    theta <- c(max(acf(x, plot = FALSE)$acf[p+1], 1e-5), rep(1 / (xmax + 1), xmax))
  }
  if(p==2){
    eacf1 <- acf(x, plot=FALSE)$acf[p+1]
    eacf2 <- acf(x, plot=FALSE)$acf[p+2]
    ealpha2 <- (eacf2-eacf1^2)/(1-eacf1^2)
    ealpha1 <- (1-ealpha2)*eacf1
    theta <- c(max(ealpha1, 1e-5), max(ealpha2, 1e-5), rep(1 / (xmax + 1), xmax))}
  if (max(x) == min(x)){
    parameters <- c(1, rep(0, p-1), 1, rep(0, xmax))
  }
  else {
    est <-suppressWarnings(constrOptim(
      theta = theta,
      f = llspinar[[p]],
      grad = NULL,
      ui = .constrmat(p, xmax),
      ci = .constrvec(p, xmax),
      dat = x
    ))
    parameters <- est$par
    parameters <- c(parameters[seq_len(p)],
                    1-sum(parameters[-seq_len(p)]),
                    parameters[-seq_len(p)])
  }
  return(parameters)
}

