#' @title (Semi)parametric INAR bootstrap procedure
#'
#' @description INAR bootstrap procedures for the semiparametric and the parametric INAR setting, where the latter allows for
#' moment- and maximum likelihood-based estimation and Poisson, geometrically and negative binomially distributed innovations.
#'
#' @param x [\code{integer}]\cr
#' vector with integer observations.
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \eqn{\code{p} \in \{1,2\}}.
#' @param B [\code{integer(1)}]\cr
#' number of bootstrap repetitions.
#' @param setting [\code{string(1)}]\cr
#' estimation setting \eqn{\in \code{\{"sp", "p"\}}}, where \code{"sp"} defines a semiparametric setting and
#' \code{"p"} a parametric setting.
#' @param type [\code{string(1)}]\cr
#' type of estimation \eqn{\in \code{\{"mom", "ml"\}}}, where \code{"mom"} performs moment-based estimation and
#' \code{"ml"} maximum likelihood-based estimation.
#' @param distr [\code{string(1)}]\cr
#' parametric family of innovation distribution \eqn{\in  \code{\{"poi", "geo", "nb"\}}}, where \code{"poi"} denotes
#' Poi(\code{lambda}), \code{"geo"} Geo(\code{prob}) and \code{"nb"} NB(\code{r}, \code{prob}) distributions.
#' @param M [\code{integer(1)}]\cr
#' upper limit for the innovations.
#' @param level [\code{numeric(1)}]\cr
#' level for the bootstrap confidence intervals (percentile interval and Hall's percentile interval
#' (bootstrap-t-interval without studentization)).
#'
#' @return (Named) List of length \code{3} containing
#'
#' - a length(\code{x})\eqn{\times}\code{B} matrix of bootstrap observations,
#'
#' - a matrix with B rows containing the bootstrap estimated parameters (In the semiparametric case,
#' each row contains the estimated coefficients \eqn{\code{alpha}_1,...,\code{alpha}_p} and the estimated entries
#' of the pmf \eqn{\code{pmf}_0, \code{pmf}_1}, ... where \eqn{\code{pmf}_i} represents the probability of
#' an innovation being equal to \eqn{i}. In the parametric case, each list entry contains the estimated coefficients
#' \eqn{\code{alpha}_1,...,\code{alpha}_p} and the estimated parameter(s) of the innovation distribution.)
#'
#' - three types of bootstrap confidence intervals (Hall, percentile, studentized).
#'
#' @examples
#' # generate data
#' dat1 <- spinar_sim(n = 200, p = 1, alpha = 0.5,
#'                    pmf = c(0.3, 0.3, 0.2, 0.1, 0.1))
#' dat2 <- spinar_sim(n = 200, p = 2, alpha = c(0.2, 0.3),
#'                    pmf = dgeom(0:60, 0.5))
#'
#' \dontrun{
#' # semiparametric INAR(1) bootstrap
#' spinar_boot(x = dat1, p = 1, B = 50, setting = "sp")
#' # parametric Geo-INAR(2) bootstrap using moment-based estimation
#' spinar_boot(x = dat2, p = 2, B = 50, setting = "p", type = "mom",
#'             distr = "geo")}
#'
#' @export spinar_boot
spinar_boot <- function(x, p, B, setting, type = NA, distr = NA, M = 100, level = 0.05){
  assert_integerish(p, lower = 1, upper = 2, len = 1)
  assert_integerish(B, lower = 1, len = 1)
  assert_integerish(x, lower = 0, min.len = p+1)
  assert_choice(setting, c("sp", "p"))
  assert_choice(type, c("mom", "ml", NA))
  assert_choice(distr, c("poi", "geo", "nb", NA))
  assert_integerish(M, lower = 0, len =  1)
  assert_numeric(level, lower = 0, upper = 1, len = 1)

  bs <- list(x_star = matrix(NA, length(x), B), parameters_star = matrix(0, B, M+p+1), bs_ci = NULL)

  if(setting=="sp"){
    parameters <- spinar_est(x, p)
    alpha_hat <- parameters[seq_len(p)]
    g_hat <- parameters[-seq_len(p)]
    for(b in 1:B){
      x_star <- spinar_sim(n = length(x), p = p, alpha = alpha_hat, pmf = g_hat)
      bs$x_star[,b] <- x_star
      parameters_star <- spinar_est(x_star, p)
      bs$parameters_star[b,1:length(parameters_star)] <- parameters_star
    }
  }

  if(setting=="p"){
    parameters <- spinar_est_param(x, p, type, distr)
    alpha_hat <- parameters[seq_len(p)]
    param_hat <- parameters[-seq_len(p)]

    if(distr=="poi"){
      for(b in 1:B){
        x_star <- spinar_sim(n = length(x), p = p, alpha = alpha_hat, pmf = dpois(0:M, param_hat[1]))
        bs$x_star[,b] <- x_star
        parameters_star <- spinar_est_param(x_star, p, type, distr)
        bs$parameters_star[b,1:length(parameters_star)] <- parameters_star
      }
    }
    if(distr=="geo"){
      for(b in 1:B){
        x_star <- spinar_sim(n = length(x), p = p, alpha = alpha_hat, pmf = dgeom(0:M, param_hat[1]))
        bs$x_star[,b] <- x_star
        parameters_star <- spinar_est_param(x_star, p, type, distr)
        bs$parameters_star[b,1:length(parameters_star)] <- parameters_star
      }
    }
    if(distr=="nb"){
      for(b in 1:B){
        x_star <- spinar_sim(n = length(x), p = p, alpha = alpha_hat, pmf = dnbinom(0:M, param_hat[1], param_hat[2]))
        bs$x_star[,b] <- x_star
        parameters_star <- spinar_est_param(x_star, p, type, distr)
        bs$parameters_star[b,1:length(parameters_star)] <- parameters_star
      }
    }
  }
  bs$parameters_star <- bs$parameters_star[,colSums(bs$parameters_star)!=0]
  bs$bs_ci <- rep(list(matrix(0, 2, 2, dimnames = list(c("lower","upper"), c("percentile","hall")))),
                  ncol(bs$parameters_star))

  for(i in 1: ncol(bs$parameters_star)){
    srt <- sort(bs$parameters_star[,i])
    if((B*level)%%2 == 0){
      bs$bs_ci[[i]][1,1] <- srt[B*level/2]
      bs$bs_ci[[i]][2,1] <- srt[B*(1-level/2)]
    } else{
      K <- ceiling((B+1)*level/2)
      bs$bs_ci[[i]][1,1] <- srt[K]
      bs$bs_ci[[i]][2,1] <- srt[B+1-K]
    }
  }

  if(ncol(bs$parameters_star)>length(parameters)){
    parameters <- c(parameters, rep(0,ncol(bs$parameters_star)-length(parameters)))
  }

  for(i in 1: ncol(bs$parameters_star)){
    srt <- sort(bs$parameters_star[,i] - parameters[i])
    if((B*level)%%2 == 0){
      bs$bs_ci[[i]][1,2] <- parameters[i] - srt[B*(1-level/2)]
      bs$bs_ci[[i]][2,2] <- parameters[i] - srt[B*level/2]
    } else{
      K <- ceiling((B+1)*level/2)
      bs$bs_ci[[i]][1,2] <- parameters[i] - srt[B+1-K]
      bs$bs_ci[[i]][2,2] <- parameters[i] - srt[K]
    }
  }

  return(bs)
}
