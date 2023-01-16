#' @title Integer Autoregressive Bootstrap Procedure
#'
#' @description
#'
#' @param x [\code{integer}]\cr
#' vector of integer values corresponding to the data
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \code{p in \{1,2\}}
#' @param B [\code{integer(1)}]\cr
#' number of bootstrap repetitions
#' @param setting [\code{string(1)}]\cr
#' setting of estimation in \code{\{"sp", "p"\}}
#' 'sp': semiparametric setting
#' 'p' : parametric setting
#' @param type [\code{string(1)}]\cr
#' type of estimation in \code{\{"mom", "ml"\}}
#' 'mom': moment estimation
#' 'ml' : maximum likelihood estimation
#' @param distr [\code{string(1)}]\cr
#' parametric family of distribution in  \code{\{'poi', 'geo', 'nb'\}}
#' \code{'poi'}: Poisson distribution with parameter \code{lambda}
#' \code{'geo'}: Geometric distribution with parameter \code{prob}
#' \code{'nb'}: Negative binomial distribution with parameters \code{r} and \code{prob}
#' @param M [\code{integer(1)}]\cr
#' upper limit for innovations
#'
#' @return bootstrap estimated parameters (INAR coefficients and pmf (or estimated innovation parameters))
#' @export
#'
#' @examples
#' ### simulating data of INAR(1)
#' # dat <- spinar_sim(n=1000, p = 1, alpha = 0.3, pmf = dpois(0:10,1.5))
#' ### semiparametric INAR bootstrap
#' # spinar_boot(dat, 1, 100)
#' ### simulating data of INAR(2)
#' # dat <- spinar_sim(n=500, p = 2, alpha = c(0.3, 0.2), pmf = dpois(0:10,1.5))
#' ### semiparametric INAR bootstrap
#' # spinar_boot(x=dat, p=2, B=100, setting="sp")
#' ### parametric Poisson INAR bootstrap
#' spinar_boot(x=dat, p=2, B=100, setting="p", type="mom", distr="poi")
#' 
spinar_boot <- function(x, p, B, setting, type=NA, distr=NA, M=100){
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  checkmate::assert_integerish(B, lower = 1, min.len = 1, max.len = 1)
  checkmate::assert_integerish(x, lower = 0, min.len = p+1)
  checkmate::assert_choice(setting, c("sp", "p"))
  checkmate::assert_choice(type, c("mom", "ml", NA))
  checkmate::assert_choice(distr, c("poi", "geo", "nb", NA))
  checkmate::assert_integerish(M, lower = 0, min.len = 1, max.len=1)
  
  if(setting=="sp"){
  parameters <- spinar_est(x, p)
  parameters_star <- list()
  alpha_hat <- parameters[seq_len(p)]
  g_hat <- parameters[-seq_len(p)]
  for(b in 1:B){
    if(p==1){
      x_star <- spinar_sim(n = length(x), p = 1, alpha = alpha_hat, pmf = g_hat)
      parameters_star[[b]] <- spinar_est(x_star, p)
    }
    if(p==2){
      x_star <- spinar_sim(n = length(x), p = 2, alpha = alpha_hat, pmf = g_hat)
      parameters_star[[b]] <- spinar_est(x_star, p)
    }
  }
  return(parameters_star)
  }
  
  if(setting=="p"){
    parameters <- spinar_est_param(x, p, type, distr)
    parameters_star <- list()
    alpha_hat <- parameters[seq_len(p)]
    param_hat <- parameters[-seq_len(p)]
    
    if(distr=="poi"){
      for(b in 1:B){
        if(p==1){
          x_star <- spinar_sim(n = length(x), p = 1, alpha = alpha_hat, pmf = dpois(0:M, param_hat[1]))
          parameters_star[[b]] <- spinar_est_param(x_star, p, type, distr)
        }
        if(p==2){
          x_star <- spinar_sim(n = length(x), p = 2, alpha = alpha_hat, pmf = dpois(0:M, param_hat[1]))
          parameters_star[[b]] <- spinar_est_param(x_star, p, type, distr)
        }
      }
    }
    if(distr=="geo"){
      for(b in 1:B){
        if(p==1){
          x_star <- spinar_sim(n = length(x), p = 1, alpha = alpha_hat, pmf = dgeom(0:M, param_hat[1]))
          parameters_star[[b]] <- spinar_est_param(x_star, p, type, distr)
        }
        if(p==2){
          x_star <- spinar_sim(n = length(x), p = 2, alpha = alpha_hat, pmf = dgeom(0:M, param_hat[1]))
          parameters_star[[b]] <- spinar_est_param(x_star, p, type, distr)
        }
      }
    }
    if(distr=="nb"){
      for(b in 1:B){
        if(p==1){
          x_star <- spinar_sim(n = length(x), p = 1, alpha = alpha_hat, pmf = dnbinom(0:M, param_hat[1], param_hat[2]))
          parameters_star[[b]] <- spinar_est_param(x_star, p, type, distr)
        }
        if(p==2){
          x_star <- spinar_sim(n = length(x), p = 2, alpha = alpha_hat, pmf = dnbinom(0:M, param_hat[1], param_hat[2]))
          parameters_star[[b]] <- spinar_est_param(x_star, p, type, distr)
        }
      } 
    }
    return(parameters_star)
  }
}
