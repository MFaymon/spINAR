#' Spinar(p)
#'
#' @param dat : dataset
#' @param p : lag
#' @param upper : upper value for constraints
#' @param par : vector with values (alpha, pmf1, ...) where alpha is the true coefficient
#'
#' @return estimated parameters
#' @export
#'
#' @examples spinar(c(2,3,1,1,1,1,1,1,3),1,3,c(0.1,0.8, 0.7, 0.6, 0.4))
#'returns 2.362001e-11 6.128583e-01 5.021233e-10 3.871417e-01
#'

#importFrom("stats", "acf", "constrOptim", "dbinom")
spinar <- function(dat, p, upper, par) {
  # auxiliar function 1
  llspinar1 <- function(par, dat) {
    Tp <- length(dat)
    alpha <- par[1]
    pmf <- par[-1]
    pmf <- c(1 - sum(pmf), pmf)
    value <- 0
    for (t in c(2:Tp)) {
      cp <-
        sum(dbinom(
          x = 0:min(dat[t], dat[t - 1]),
          size = dat[t - 1],
          prob = alpha
        )
        * pmf[dat[t] +  1 - (0:min(dat[t], dat[t - 1]))])
      value <- value - log(cp)
    }
    value
  }
  # auxiliar function 2
  constrmat <- function(p, upper) {
    mat <- array(0, c(p + upper + 2, p + upper))
    mat[1:(p + upper), 1:(p + upper)] <- diag(1, p + upper)
    mat[p + upper + 1, 1:p] <- (-1)
    mat[p + upper + 2, (p + 1):(p + upper)] <- (-1)
    mat
  }

  # auxiliar function 3
  constrvec <- function(p, upper) {
    vec <- rep(0, p + upper + 2)
    vec[p + upper + 1] <- (-1)
    vec[p + upper + 2] <- (-1)
    vec
  }
  eacf1 <-
    ifelse(acf(dat, plot = FALSE)$acf[2] <= 0,
           0.00001,
           acf(dat, plot = FALSE)$acf[2])
  xmax <- max(dat)
  est <-
    suppressWarnings(constrOptim(
      c(eacf1, rep(1 / (xmax + 1), xmax)),
      llspinar1,
      NULL,
      ui = constrmat(1, xmax),
      ci = constrvec(1, xmax),
      dat = dat
    ))
  parameters <- est$par
  return(parameters)
}
