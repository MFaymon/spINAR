#' @title Penalized semiparametric estimation of INAR models
#'
#' @description
#' Semiparametric penalized estimation of the autoregressive parameters and the innovation distribution of INAR(\code{p}) models,
#' \eqn{\code{p} \in \{1,2\}}. The estimation is conducted by maximizing the penalized conditional likelihood of the model. If both
#' penalization parameters are set to zero, the function coincides to the spinar_est function of this package.
#'
#' @param x [\code{integer}]\cr
#' vector with integer observations.
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \eqn{\code{p} \in \{1,2\}}.
#' @param penal1
#' \eqn{L_1} penalization parameter (default value zero results in no \eqn{L_1} penalization)
#' @param penal2
#' \eqn{L_2} penalization parameter (default value zero results in  no \eqn{L_2} penalization)
#'
#' @return Vector containing the penalized estimated coefficients \eqn{\code{alpha}_1,...,\code{alpha}_p} and the penalized
#' estimated entries of the pmf \eqn{\code{pmf}_0, \code{pmf}_1},... where \eqn{\code{pmf}_i} represents the probability of
#' an innovation being equal to \eqn{i}.
#'
#' @examples
#' # generate data
#' dat1 <- spinar_sim(n = 50, p = 1, alpha = 0.5,
#'                    pmf = c(0.3, 0.25, 0.2, 0.15, 0.1))
#'
#' # penalized semiparametric estimation
#' spinar_penal(x = dat1, p = 1, penal1 = 0, penal2 = 0.1)
#'
#' @export spinar_penal
spinar_penal <- function(x, p, penal1 = 0, penal2 = 0) {
  assert_integerish(p, lower = 1, upper = 2, len = 1, any.missing = FALSE)
  assert_integerish(x, lower = 0, min.len = p+1)
  assert_numeric(penal1, len = 1)
  assert_numeric(penal2, len = 1)
  xmax <- max(x)
  if(p==1){
    theta <- c(max(acf(x, plot = FALSE)$acf[p+1], 1e-5), rep(1 / (xmax + 1), xmax))
  }
  if(p==2){
    eacf1 <- acf(x, plot=FALSE)$acf[p+1]
    eacf2 <- acf(x, plot=FALSE)$acf[p+2]
    ealpha2 <- max((eacf2-eacf1^2)/(1-eacf1^2), 1e-16)
    ealpha1 <- max((1-ealpha2)*eacf1, 1e-16)
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




