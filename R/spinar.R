#' Spinar(p)
#'
#' @param dat : dataset
#' @param p : lag
#'
#' @return (alpha, pmf0, pmf1, ...) estimated parameters
#' @export
#'
#' @examples spinar(c(2,3,1,1,1,1,1,1,3), 1)
#'2.362001e-11 4.205347e-09 6.128583e-01 5.021233e-10 3.871417e-01
#'
spinar <- function(dat, p) {
  # constraints for input
  assert_integerish(p,lower = 1, min.len = 1, max.len = 1)
  assert_integerish(dat, min.len = p+1) # further requirements on input?
  xmax <- max(dat)
  est <-
    suppressWarnings(constrOptim(
      c(pmax(1e-5, acf(dat, plot = FALSE)$acf[seq_len(p)+1]), rep(1 / (xmax + 1), xmax)),
      llspinar[p],
      NULL,
      ui = .constrmat(p, xmax),
      ci = .constrvec(p, xmax),
      dat = dat
    ))
  parameters <- est$par
  parameters <- c(parameters[seq_len(p)],
                  1-sum(parameters[-seq_len(p)]),
                  parameters[-seq_len(p)])
  return(parameters)
}




