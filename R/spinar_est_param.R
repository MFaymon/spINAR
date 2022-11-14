# parametric estimation of INAR models
# for now only moment-based

spinar_est_param <- function(x, p, type, distr){
  # to do: 
  # ensure that p is either 1 or 2, ifnot issue warning
  # ensure that data is only integer, ifnot warning
  # ensure that type is either mom or ml
  # ensure that distr is either poi, geo or nb
  # in documentation: parameters of poi: lambda
  # in documentation: parameters of geo: prob
  # in documentation: parameters of nb: r, prob
  
  if(type=="mom"){
    # ensure that the following 4 values are inbetween 0 and 1
    eacf1 <-acf(x, plot=FALSE)$acf[2]
    eacf2 <- acf(x, plot=FALSE)$acf[3]
    ealpha2 <- (eacf2-eacf1^2)/(1-eacf1^2)
    ealpha1 <- (1-ealpha2)*eacf1
  
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
dat6 <- nbinar2(1000, 0.3, 0.5, 5, 0.16)

spinar_est_param(dat1,1,"mom","poi")
spinar_est_param(dat2,2,"mom","poi")
spinar_est_param(dat3,1,"mom","geo")
spinar_est_param(dat4,2,"mom","geo")
spinar_est_param(dat5,1,"mom","nb")
spinar_est_param(dat6,2,"mom","nb")




