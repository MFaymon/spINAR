#' @title Semiparametric estimation of INAR models
#'
#' @description
#' Semiparametric estimation of the autoregressive parameters and the innovation
#' distribution of INAR(\code{p}) models, \eqn{\code{p} \in \{1,2\}}. The
#' estimation is conducted by maximizing the conditional likelihood of the model.
#'
#' @param x [\code{integer}]\cr
#' vector with integer observations.
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \eqn{\code{p} \in \{1,2\}}.
#'
#' @return Vector containing the estimated coefficients \eqn{\code{alpha}_1,...,\code{alpha}_p} and the estimated entries
#' of the pmf \eqn{\code{pmf}_0,..., \code{pmf}_k} where \eqn{\code{pmf}_i} represents the probability of
#' an innovation being equal to \eqn{i}.
#'
#' @examples
#' # generate data
#' dat1 <- spinar_sim(n = 200, p = 1, alpha = 0.5,
#'                    pmf = c(0.3, 0.3, 0.2, 0.1, 0.1))
#' dat2 <- spinar_sim(n = 200, p = 2, alpha = c(0.2, 0.3),
#'                    pmf = c(0.25, 0.2, 0.15, 0.1, 0.1, 0.1, 0.1))
#'
#' \dontrun{
#' # semiparametric estimation of INAR(1) model
#' spinar_est(x = dat1, p = 1)
#' # semiparametric estimation of INAR(2) model
#' spinar_est(x = dat2, p = 2)}
#'
#' @export spinar_est
spinar_est <- function(x, p) {
  assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  assert_integerish(x, min.len = p+1, lower = 0)
  xmax <- max(x)
  if(p==1){
    theta <- c(max(acf(x, plot = FALSE)$acf[p+1], 1e-5), rep(1 / (xmax + 1), xmax))
  }
  if(p==2){
    eacf1 <- max(acf(x, plot=FALSE)$acf[2], 1e-16)
    eacf2 <- max(acf(x, plot=FALSE)$acf[3], 1e-16)
    ealpha2 <- max((eacf2-eacf1^2)/(1-eacf1^2), 1e-16)
    ealpha1 <- max((1-ealpha2)*eacf1, 1e-16)
    theta <- c(ealpha1, ealpha2, rep(1 / (xmax + 1), xmax))}
  if (max(x) == min(x)){
    parameters <- c(1, rep(0, p-1), 1, rep(0, xmax))
  }
  else {
    est <- suppressWarnings(constrOptim(
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

