#' @title Semiparametric Integer Autoregressive Bootstrap Procedure
#'
#' @description
#'
#' @param x [\code{integer}]\cr
#' vector of integer values corresponding to the data
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \code{p in \{1,2\}}
#' @param B [\code{integer(1)}]\cr
#' number of bootstrap repetitions
#'
#' @return bootstrap estimated parameters (INAR coefficients and pmf)
#' @export
#'
#' @examples
#' ### simulating data of INAR(1)
#' dat <- spinar_sim(n=1000, p = 1, alpha = 0.3, pmf = dpois(0:10,1.5))
#' ### semiparametric INAR bootstrap
#' spinar_boot(dat, 1, 100)
#' ### simulating data of INAR(2)
#' dat <- spinar_sim(n=500, p = 2, alpha = c(0.3, 0.2), pmf = dpois(0:10,1.5))
#' ### semiparametric INAR bootstrap
#' spinar_boot(dat, 2, 100)
spinar_boot <- function(x, p, B){ #later: add arguments about which estimation the user wants to perform
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  checkmate::assert_integerish(B, lower = 1, min.len = 1, max.len = 1)
  checkmate::assert_integerish(x, min.len = p+1)
  parameters <- spinar_est(x, p)
  parameters_star <- list()
  alpha_hat <- parameters[seq_len(p)]
  g_hat <- parameters[-seq_len(p)]
  for(b in 1:B){
    if(p==1){
      x_star <- spinar_sim(n = length(x), p=1, alpha = alpha_hat, pmf = g_hat)
      parameters_star[[b]] <- spinar_est(x_star, p)
    }
    if(p==2){
      x_star <- spinar_sim(n = length(x), p = 2, alpha = alpha_hat, pmf = g_hat)
      parameters_star[[b]] <- spinar_est(x_star, p)
    }
  }
  return(parameters_star)
}




