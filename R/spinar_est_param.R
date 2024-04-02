#' @title Parametric estimation of INAR models
#'
#' @description Parametric estimation of the autoregressive parameters and the innovation distribution of INAR(\code{p}) models,
#' \eqn{\code{p} \in \{1,2\}}, with Poisson, geometrically or negative binomially distributed innovations. The estimation can either be
#' moment- or maximum likelihood-based.
#'
#' @param x [\code{integer}]\cr
#' vector with integer observations.
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \eqn{\code{p} \in \{1,2\}}.
#' @param type [\code{string(1)}]\cr
#' type of estimation \eqn{\in \code{\{"mom", "ml"\}}}, where \code{"mom"} performs moment-based estimation and
#' \code{"ml"} maximum likelihood-based estimation.
#' @param distr [\code{string(1)}]\cr
#' parametric family of innovation distribution \eqn{\in  \code{\{'poi', 'geo', 'nb'\}}}, where \code{"poi"} denotes
#' Poi(\code{lambda}), \code{"geo"} Geo(\code{prob}) and \code{"nb"} NB(\code{r}, \code{prob}) distributions.
#'
#' @return Named vector containing the estimated coefficients \eqn{\code{alpha}_1,...,\code{alpha}_p} and the estimated parameter(s)
#' of the innovation distribution.
#'
#' @examples
#' # generate data
#' # Poi-INAR(1) data
#' dat1 <- spinar_sim(n = 200, p = 1, alpha = 0.5, pmf = dpois(0:20, 1))
#' # Geo-INAR(2) data
#' dat2 <- spinar_sim(n = 200, p = 2, alpha = c(0.2, 0.3),
#'                    pmf = dgeom(0:60, 0.5))
#' # NB-INAR(1) data
#' dat3 <- spinar_sim(n = 200, p = 1, alpha = 0.5, pmf = dnbinom(0:40, 2, 2/3))
#'
#' # moment-based parametric estimation of Poi-INAR(1) model
#' spinar_est_param(x = dat1, p = 1, type = "mom", distr = "poi")
#' # moment-based parametric estimation of Geo-INAR(2) model
#' spinar_est_param(x = dat2, p = 2, type = "mom", distr = "geo")
#' # maximum likelihood-based parametric estimation of NB-INAR(1) model
#' spinar_est_param(x = dat3, p = 1, type = "ml", distr = "nb")
#'
#' @export spinar_est_param
spinar_est_param <- function(x, p, type, distr){
  assert_integerish(p, lower = 1, upper = 2, len = 1, any.missing = FALSE)
  assert_integerish(x, min.len = p+1, lower = 0)
  assert(checkChoice(type, c("mom", "ml")))
  assert(checkChoice(distr, c("poi", "geo", "nb")))
  eacf1 <- max(acf(x, plot=FALSE)$acf[2], 1e-16)
  eacf2 <- max(acf(x, plot=FALSE)$acf[3], 1e-16)
  ealpha2 <- max((eacf2-eacf1^2)/(1-eacf1^2), 1e-16)
  ealpha1 <- max((1-ealpha2)*eacf1, 1e-16)

  if(type=="mom"){
    if(p==1 && distr=="poi"){
      if(max(x)==min(x)){
        param <- c("alpha1"=1, "lambda"=0)
      } else{
        alpha1_hat <- eacf1
        lambda <- max(mean(x)*(1-alpha1_hat),0)
        param <- c("alpha1"=alpha1_hat, "lambda"=lambda)
      }
    }
    if(p==2 && distr=="poi"){
      if(max(x)==min(x)){
        param <- c("alpha1"=1, "alpha2"=0, "lambda"=0)
      } else{
        alpha1_hat <- ealpha1
        alpha2_hat <- ealpha2
        lambda <- max(mean(x)*(1-alpha1_hat-alpha2_hat),0)
        param <- c("alpha1"=alpha1_hat,"alpha2"=alpha2_hat, "lambda"=lambda)
      }
    }
    if(p==1 && distr=="geo"){
      if(max(x)==min(x)){
        param <- c("alpha1"=1, "prob"=1)
      } else{
        alpha1_hat <- eacf1
        prob <- max(min(1/(mean(x)*(1-alpha1_hat)+1),0.99),0)
        param <- c("alpha1"=alpha1_hat, "prob"=prob)
      }
    }
    if(p==2 && distr=="geo"){
      if(max(x)==min(x)){
        param <- c("alpha1"=1, "alpha2"=0, "prob"=1)
      } else{
        alpha1_hat <- ealpha1
        alpha2_hat <- ealpha2
        prob <- max(min(1/(mean(x)*(1-(alpha1_hat+alpha2_hat))+1),0.99),0)
        param <- c("alpha1"=alpha1_hat, "alpha2"=alpha2_hat, "prob"=prob)
      }
    }
    if(p==1 && distr=="nb"){
      if(max(x)==min(x)){
        param <- c("alpha1"=1, "r" = 1, "prob"=1)
      } else{
        alpha1_hat <- eacf1
        prob <- max(min(mean(x)/(var(x)*(1+alpha1_hat)-alpha1_hat*mean(x)),0.99),0)
        r <- round((mean(x)*prob*(1-alpha1_hat))/(1-prob))
        param <- c("alpha1"=alpha1_hat, "r"=r, "prob"=prob)
      }
    }
    if(p==2 && distr=="nb"){
      if(max(x)==min(x)){
        param <- c("alpha1"=1, "alpha2"=0, "r" = 1, "prob"=1)
      } else{
        alpha1_hat <- ealpha1
        alpha2_hat <- ealpha2
        prob <- max(min(mean(x)/(var(x)*(1+alpha1_hat+alpha2_hat)-(alpha1_hat +alpha2_hat)*mean(x)),0.99),0)
        r <- round((mean(x)*prob*(1-alpha1_hat-alpha2_hat))/(1-prob))
        param <- c("alpha1"=alpha1_hat, "alpha2"=alpha2_hat, "r"=r, "prob"=prob)
      }
    }
  }
  if(type=="ml"){
    if(distr=="poi"){
      if (max(x) == min(x)){
        ifelse(p==1, param <- c("alpha1"=1, "lambda"=0), param <- c("alpha1"=1, "alpha2"=0, "lambda"=0))
      }
      else {
        if(p==1){
          theta <- c(max(eacf1, 1e-5), max(mean(x)*(1-eacf1),0))
        }
        if(p==2){
          theta <- c(max(ealpha1, 1e-5), max(ealpha2, 1e-5), max(mean(x)*(1-ealpha1-ealpha2),0))
        }
        est <- suppressWarnings(constrOptim(
          theta = theta,
          f = llpinar_poi[[p]],
          grad = NULL,
          ui = .constrmat_poi(p),
          ci = .constrvec_poi(p),
          dat = x
        ))
        parameters <- est$par
        ifelse(p==1, param <- c("alpha1"=parameters[1], "lambda"=parameters[2]),
               param <- c("alpha1"=parameters[1], "alpha2"=parameters[2], "lambda"=parameters[3]))
      }
    }
    if(distr=="geo"){
      if (max(x) == min(x)){
        ifelse(p==1, param <- c("alpha1"=1, "prob"=1), param <- c("alpha1"=1, "alpha2"=0, "prob"=1))
      }
      else {
        if(p==1){
          theta <- c(max(eacf1, 1e-5), max(min(1/(mean(x)*(1-eacf1)+1),0.99),0))
        }
        if(p==2){
          theta <- c(max(ealpha1, 1e-5), max(ealpha2, 1e-5), max(min(1/(mean(x)*(1-(ealpha1+ealpha2))+1),0.99),0))
        }
        est <- suppressWarnings(constrOptim(
          theta = theta,
          f = llpinar_geo[[p]],
          grad = NULL,
          ui = .constrmat_geo(p),
          ci = .constrvec_geo(p),
          dat = x
        ))
        parameters <- est$par
        ifelse(p==1, param <- c("alpha1"=parameters[1], "prob"=parameters[2]),
               param <- c("alpha1"=parameters[1], "alpha2"=parameters[2], "prob"=parameters[3]))
      }
    }
    if(distr=="nb"){
      if (max(x) == min(x)){
        ifelse(p==1, param <- c("alpha1"=1, "r"=1, "prob"=1), param <- c("alpha1"=1, "alpha2"=0, "r"=1, "prob"=1))
      }
      else {
        if(p==1){
          prob <- max(min(mean(x)/(var(x)*(1+eacf1)-eacf1*mean(x)),0.99),0)
          rs <- ifelse(round((mean(x)*prob*(1-eacf1))/(1-prob)) == 0, 1, round((mean(x)*prob*(1-eacf1))/(1-prob)))
          theta <- c(max(eacf1, 1e-5), rs, prob)
        }
        if(p==2){
          prob <- max(min(mean(x)/(var(x)*(1+ealpha1+ealpha2)-(ealpha1+ealpha2)*mean(x)),0.99),0)
          rs <- ifelse(round((mean(x)*prob*(1-ealpha1-ealpha2))/(1-prob)) == 0, 1, round((mean(x)*prob*(1-ealpha1-ealpha2))/(1-prob)))
          theta <- c(max(ealpha1, 1e-5), max(ealpha2, 1e-5), rs, prob)
        }
        est <- suppressWarnings(constrOptim(
          theta = theta,
          f = llpinar_nb[[p]],
          grad = NULL,
          ui = .constrmat_nb(p),
          ci = .constrvec_nb(p),
          dat = x
        ))
        parameters <- est$par
        ifelse(p==1, param <- c("alpha1"=parameters[1], "r"=round(parameters[2]), "prob"=parameters[3]),
               param <- c("alpha1"=parameters[1], "alpha2"=parameters[2], "r"=round(parameters[3]), "prob"=parameters[4]))
      }
    }
  }
  if (p==2){
    assert_numeric(param[['alpha1']], upper = 1-param[['alpha2']])
  }
  return(param)
}
