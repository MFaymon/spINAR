#' @title Semiparametric Integer Autoregressive Boostrap model.
#'
#' @description
#'
#' @param x [\code{integer}]\cr
#' vector of integer values corresponding to the data used for estimate the parameter using `spinar_est` function.
#' @param p [\code{integer(1)}]\cr
#' lag of the INAR(p) where \code{p in \{1,2\}}
#' @param B [\code{integer(1)}]\cr
#'
#' @return parameters
#' @export
#'
#' @examples
#' ### simulating data of INAR(1)
#' dat <- spinar_sim(n=1000, p = 1, alpha = 0.3, pmf = dpois(0:10,1.5))
#' ## Spinar bootstrap
#' spinar_boot(dat, 1, 10)
#' ### simulating data of INAR(2)
#' dat <- spinar_sim(n=500, p = 2, alpha = c(0.3, 0.2), pmf = dpois(0:10,1.5))
#' spinar_boot(dat, 2, 10)
spinar_boot <- function(x, p, B){ #later: add arguments about which estimation the user wants to perform
  parameters <- spinar_est(x, p)
  parameters_star <- list()
  alpha_hat <- parameters[p]
  g_hat <- parameters[-p]
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




