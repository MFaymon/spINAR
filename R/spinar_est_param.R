#' Parametric estimation of INAR models
#' @description for now only moment-based
#' @param x [\code{integer}]\cr
#' vector of integer values corresponding to the data
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \code{p in \{1,2\}}
#' @param type
#' @param distr
#'
#' @return
#' @export
#'
#' @examples

spinar_est_param <- function(x, p, type, distr){
  # to do:
  # ensure that p is either 1 or 2, ifnot issue warning
  # ensure that data is only integer, ifnot warning
  # ensure that type is either mom or ml
  # ensure that distr is either poi, geo or nb
  # in documentation: parameters of poi: lambda
  # in documentation: parameters of geo: prob
  # in documentation: parameters of nb: r, prob
  
  # ensure that the following 4 values are inbetween 0 and 1
  eacf1 <- max(acf(x, plot=FALSE)$acf[2], 1e-16)
  eacf2 <- max(acf(x, plot=FALSE)$acf[3], 1e-16)
  ealpha2 <- max((eacf2-eacf1^2)/(1-eacf1^2), 1e-16)
  ealpha1 <- max((1-ealpha2)*eacf1, 1e-16)
  
  if(type=="mom"){
    if(p==1 && distr=="poi"){
      alpha1_hat <- eacf1
      lambda <- mean(x)*(1-alpha1_hat)
      param <- c("alpha1"=alpha1_hat, "lambda"=lambda)
    }
    if(p==2 && distr=="poi"){
      alpha1_hat <- ealpha1
      alpha2_hat <- ealpha2
      lambda <- mean(x)*(1-alpha1_hat-alpha2_hat)
      param <- c("alpha1"=alpha1_hat,"alpha2"=alpha2_hat, "lambda"=lambda)
    }
    if(p==1 && distr=="geo"){
      alpha1_hat <- eacf1
      prob <- 1/(mean(x)*(1-alpha1_hat)+1)
      param <- c("alpha1"=alpha1_hat, "prob"=prob)
    }
    if(p==2 && distr=="geo"){
      alpha1_hat <- ealpha1
      alpha2_hat <- ealpha2
      prob <- 1/(mean(x)*(1-(alpha1_hat+alpha2_hat))+1)
      param <- c("alpha1"=alpha1_hat, "alpha2"=alpha2_hat, "prob"=prob)
    }
    if(p==1 && distr=="nb"){
      alpha1_hat <- eacf1
      prob <- mean(x)/(var(x)*(1+alpha1_hat)-alpha1_hat*mean(x))
      r <- round((mean(x)*prob*(1-alpha1_hat))/(1-prob))
      param <- c("alpha1"=alpha1_hat, "r"=r, "prob"=prob)
    }
    if(p==2 && distr=="nb"){
      alpha1_hat <- ealpha1
      alpha2_hat <- ealpha2
      prob <- mean(x)/(var(x)*(1+alpha1_hat+alpha2_hat)-(alpha1_hat +alpha2_hat)*mean(x))
      r <- round((mean(x)*prob*(1-alpha1_hat-alpha2_hat))/(1-prob))
      param <- c("alpha1"=alpha1_hat, "alpha2"=alpha2_hat, "r"=r, "prob"=prob)
    }
    #warning if alpha_1 + alpha_2 > 1?
  }
  
  if(type=="ml"){
    if(distr=="poi"){
      if(p==1){
        theta <- c(max(eacf1, 1e-5), mean(x)*(1-eacf1))
      }
      if(p==2){
        theta <- c(max(ealpha1, 1e-5), max(ealpha2, 1e-5), mean(x)*(1-ealpha1-ealpha2))
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
    if(distr=="geo"){
      if(p==1){
        theta <- c(max(eacf1, 1e-5), 1/(mean(x)*(1-eacf1)+1))
      }
      if(p==2){
        theta <- c(max(ealpha1, 1e-5), max(ealpha2, 1e-5), 1/(mean(x)*(1-(ealpha1+ealpha2))+1))
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
    if(distr=="nb"){
      if(p==1){
        prob <- mean(x)/(var(x)*(1+eacf1)-eacf1*mean(x))
        theta <- c(max(eacf1, 1e-5), round((mean(x)*prob*(1-eacf1))/(1-prob)), prob)
      }
      if(p==2){
        prob <- mean(x)/(var(x)*(1+ealpha1+ealpha2)-(ealpha1+ealpha2)*mean(x))
        theta <- c(max(ealpha1, 1e-5), max(ealpha2, 1e-5), round((mean(x)*prob*(1-ealpha1-ealpha2))/(1-prob)), prob)
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
  
  return(param)
}



## Examples

geominar1 <- function(n, alpha, pr){
  err <- rgeom(n, pr)
  x <- numeric(n)
  #initialization x_0 = 0
  x[1] <- err[1]
  for(i in 2:n){
    x[i] <- rbinom(1, x[i-1], alpha) + err[i]
  }
  return(x)
}

geominar2 <- function(n, alpha1, alpha2, pr){
  err <- rgeom(n, pr)
  x <- numeric(n)
  #initialization x_0 = 0
  x[1] <- err[1]
  x[2] <- x[1] + err[2]
  for(i in 3:n){
    x[i] <- rbinom(1, x[i-1], alpha1) + rbinom(1, x[i-2], alpha2) + err[i]
  }
  return(x)
}

nbinar1 <- function(n, alpha, nsize, pi){
  err <- rnbinom(n, nsize, pi)
  x <- numeric(n)
  #initialize x_0 = 0
  x[1] <- err[1]
  for(i in 2:n){
    x[i] <- rbinom(1, x[i-1], alpha) + err[i]
  }
  return(x)
}

nbinar2 <- function(n, alpha1, alpha2, nsize, pi){
  err <- rnbinom(n, nsize, pi)
  x <- numeric(n)
  #initialize x_0 = 0
  x[1] <- err[1]
  x[2] <- x[1] + err[2]
  for(i in 3:n){
    x[i] <- rbinom(1, x[i-1], alpha1) + rbinom(1, x[i-2], alpha2) + err[i]
  }
  return(x)
}

pinar1 <- function(n, alpha, lambda){
  err <- rpois(n, lambda)
  x <- numeric(n)
  #initialization x_0 = 0
  x[1] <- err[1]
  for(i in 2:n){
    x[i] <- rbinom(1, x[i-1], alpha) + err[i]
  }
  return(x)
}

pinar2 <- function(n, alpha1, alpha2, lambda){
  err <- rpois(n, lambda)
  x <- numeric(n)
  #initialization
  x[1] <- err[1]
  x[2] <- x[1] + err[2]
  for(i in 3:n){
    x[i] <- rbinom(1, x[i-1], alpha1) + rbinom(1, x[i-2], alpha2) + err[i]
  }
  return(x)
}


dat1 <- pinar1(1000,0.3, 1.4)
dat2 <- pinar2(1000,0.3, 0.5, 0.8)
dat3 <- geominar1(1000,0.3, 0.5)
dat4 <- geominar2(1000, 0.3, 0.15, 0.5)
dat5 <- nbinar1(1000, 0.4, 5, 0.33)
dat6 <- nbinar2(1000, 0.3, 0.5, 5, 0.16) #extremely high running time -> exclude?

spinar_est_param(dat1,1,"mom","poi")
spinar_est_param(dat1,1,"ml","poi")
spinar_est_param(dat2,2,"mom","poi")
spinar_est_param(dat2,2,"ml","poi")
spinar_est_param(dat3,1,"mom","geo")
spinar_est_param(dat3,1,"ml","geo")
spinar_est_param(dat4,2,"mom","geo")
spinar_est_param(dat4,2,"ml","geo")
spinar_est_param(dat5,1,"mom","nb")
spinar_est_param(dat5,1,"ml","nb")
spinar_est_param(dat6,2,"mom","nb")
spinar_est_param(dat6,2,"ml","nb")




