#' @title Penalized Semiparametric Estimation of INAR(p) Model.
#'
#' @description
#' Performs a penalized semiparametric estimation of the autoregressive parameters and the innovation distribution for INAR models of order \code{p},
#' \code{p in {1,2}}, the estimation is conducted by maximizing the penalized conditional likelihood of the model.
#'
#' @param x [\code{integer}]\cr
#' vector of integer values corresponding to the data
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \code{p in \{1,2\}}
#' @param penal1
#' L1 penalization parameter (default value zero meaning no L1 penalization)
#' @param penal2
#' L2 penalization parameter (default value zero meaning no L2 penalization)
#'
#' @return estimated parameters \code{(alpha_1, ..., alpha_p, pmf[0], pmf[1], ...)},
#' where \code{(alpha_1, ..., alpha_p)} are the estimated autoregressive coefficients
#' and \code{(pmf[0], pmf[1], ...)} are the estimated entries of the probability mass function of the innovation distribution,
#' where \code{pmf[i]} denotes the probability of observing value i
#' @export
#'
#' @examples
#' ### data generation
#' # dat <- spinar_sim(100, 1, 0.5, dpois(0:20,1))
#' ## penalized semiparametric estimation
#' # spinar_penal(dat, 1, 0.2, 0.4)
spinar_penal <- function(x, p, penal1=0, penal2=0) {
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  checkmate::assert_integerish(x, lower =0, min.len = p+1)
  checkmate::assert_numeric(penal1, len = 1)
  checkmate::assert_numeric(penal2, len = 1)
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
      f = llspinar_penal[[p]],
      grad = NULL,
      ui = .constrmat(p, xmax),
      ci = .constrvec(p, xmax),
      dat = x,
      penal1 = penal1,
      penal2 = penal2
    ))
    parameters <- est$par
    parameters <- c(parameters[seq_len(p)],
                    1-sum(parameters[-seq_len(p)]),
                    parameters[-seq_len(p)])
  }
  return(parameters)
}




