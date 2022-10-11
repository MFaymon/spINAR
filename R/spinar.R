#' spinar(p)
#'
#' @param dat : dataset
#' @param p : lag
#'
#' @return (alpha_1,..., alpha_p, pmf0, pmf1, ...) estimated parameters
#' @export
#'
#' @examples spinar(c(2,3,1,1,1,1,1,1,3), 1)
#'2.362001e-11 4.205347e-09 6.128583e-01 5.021233e-10 3.871417e-01
#'

spinar <- function(dat, p) {
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1)
  checkmate::assert_integerish(dat, min.len = p+1) # further requirements on input?
  xmax <- max(dat)
  if (max(dat) == min(dat)){
    parameters <- c(c(1, 0)[0: 1L + (p==2)],
                    c(rep(0, xmax),1))
  }
  else {
  est <-
    suppressWarnings(constrOptim(
      theta = c(pmax(1e-5, acf(dat, plot = FALSE)$acf[seq_len(p)+1]), rep(1 / (xmax + 1), xmax)),
      f = llspinar[[p]],
      grad = NULL,
      ui = .constrmat(p, xmax),
      ci = .constrvec(p, xmax),
      dat = dat
    ))
  parameters <- est$par
  parameters <- c(parameters[seq_len(p)],
                  1-sum(parameters[-seq_len(p)]),
                  parameters[-seq_len(p)])
  }
  return(parameters)
}

