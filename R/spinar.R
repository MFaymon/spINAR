#' @title Parameter estimation of (semi)parametric integer autoregressive models
#'
#' @description
#' Performs bla bla.... EXPLAIN IN DETAIL!
#'
#' @param x [\code{integer}]\cr
#' EXPLAIN IN DETAIL!.
#' @param p [\code{integer(1)}]\cr
#' Lag. EXPLAIN IN DETAIL!
#'
#' @return [\code{numeric(p+??)}]\cr
#' Estimated parameters \code{(alpha_1,..., alpha_p, pmf0, pmf1, ...)}. EXPLAIN IN DETAIL!
#'
#' @examples
#' # EXPLAIN IN DETAIL!
#' spinar(c(2,3,1,1,1,1,1,1,3), 1)
#' # 2nd EXAMPLE!
#' @export

spinar <- function(x, p) {
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  checkmate::assert_integerish(x, min.len = p+1)
  xmax <- max(x)
  if (max(x) == min(x)){
    parameters <- c(1, rep(0, p-1), 1, rep(0, xmax))
  }
  else {
  est <-
    suppressWarnings(constrOptim( # catch warning and pass over?
      theta = c(pmax(1e-5, acf(x, plot = FALSE)$acf[seq_len(p)+1]), rep(1 / (xmax + 1), xmax)),
      f = llspinar[[p]],
      grad = NULL,
      ui = .constrmat(p, xmax),
      # substracting the corresponding smallest value
      # to be in the feasible region ui %*% theta - ci >= 0
      ci = .constrvec(p, xmax) + min(0, - 1e-5 + .constrmat(p, xmax) %*% c(pmax(1e-5, acf(x, plot = FALSE)$acf[seq_len(p)+1]), rep(1 / (xmax + 1), xmax)) - .constrvec(p, xmax)),
      dat = x
    ))
  parameters <- est$par
  parameters <- c(parameters[seq_len(p)],
                  1-sum(parameters[-seq_len(p)]),
                  parameters[-seq_len(p)])
  }
  return(parameters)
}

