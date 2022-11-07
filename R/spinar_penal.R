#' @title Penalized (semiparametric) estimation of INAR(p) model.
#'
#' @description
#'
#' @param x [\code{integer}]\cr
#' vector of integer values corresponding to the data.
#' @param p [\code{integer(1)}]\cr
#' lag of the INAR(p) where \code{p in \{1,2\}}.
#' @param penal1
#' default value penal1=0 meaning no penalization.
#' @param penal2
#' default value penal2=0 meaning no penalization.
#'
#' @return parameters
#' @export
#'
#' @examples
#' ### data generated
#' dat <- spinar_sim(n = 100, p=1, alpha = 0.5, pmf = dpois(0:20,1))
#' ## penalized spinar
#' spinar_penal(dat,1,0.5,0.5)
spinar_penal <- function(x, p, penal1=0, penal2=0) {
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  checkmate::assert_integerish(x, min.len = p+1)
  checkmate::assert_numeric(penal1, len = 1)
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




