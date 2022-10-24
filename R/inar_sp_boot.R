spinar_boot <- function(x, p, B){ #later: add arguments about which estimation the user wants to perform 
  parameters <- spinar(x, p)
  parameters_star <- list()
  for(b in 1:B){
    if(p==1){
      alpha_hat <- parameters[p]
      g_hat <- parameters[-p]
      x_star <- sp_inar1(length(x), alpha_hat, g_hat)
      parameters_star[[b]] <- spinar(x_star, p)
    }
    if(p==2){
      alpha_hat <- parameters[seq_len(p)]
      g_hat <- parameters[-seq_len(p)]
      x_star <- sp_inar2(length(x), alpha_hat[1], alpha_hat[2], g_hat)
      parameters_star[[b]] <- spinar(x_star, p)
    }
  }
  return(parameters_star)
}

##########################################
# Small running example
##########################################

## functions needed 

spinar <- function(x, p) {
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  checkmate::assert_integerish(x, min.len = p+1)
  xmax <- max(x)
  if(p==1){
    theta <- c(max(acf(x, plot = FALSE)$acf[p+1], 1e-5), rep(1 / (xmax + 1), xmax))
  }
  if(p==2){
    eacf1 <- acf(x, plot=FALSE)$acf[p+1]
    eacf2 <- acf(x, plot=FALSE)$acf[p+2]
    ealpha2 <- (eacf2-eacf1^2)/(1-eacf1^2)
    ealpha1 <- (1-ealpha2)*eacf1
    theta <- c(max(ealpha1, 1e-5), max(ealpha2, 1e-5), rep(1 / (xmax + 1), xmax))}
  if (max(x) == min(x)){
    parameters <- c(1, rep(0, p-1), 1, rep(0, xmax))
  }
  else {
    est <-suppressWarnings(constrOptim(
      theta = theta,
      f = llspinar[[p]],
      grad = NULL,
      ui = .constrmat(p, xmax),
      ci = .constrvec(p, xmax),
      dat = x
    ))
    parameters <- est$par
    parameters <- c(parameters[seq_len(p)],
                    1-sum(parameters[-seq_len(p)]),
                    parameters[-seq_len(p)])
  }
  return(parameters)
}

.constrmat <- function(p, upper) {
  mat <- array(0, c(p + upper + 2, p + upper))
  mat[1:(p + upper), 1:(p + upper)] <- diag(1, p + upper)
  mat[p + upper + 1, 1:p] <- (-1)
  mat[p + upper + 2, (p + 1):(p + upper)] <- (-1)
  mat
}

.constrvec <- function(p, upper) {
  vec <- rep(0, p + upper + 2)
  vec[p + upper + 1] <- (-1)
  vec[p + upper + 2] <- (-1)
  vec
}

llspinar <- list(
  function(par, dat) {
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
        * pmf[dat[t] +  1 - (0:min(dat[t], dat[t-1]))])
      value <- value - log(cp)
    }
    value
  },
  function(par, dat) {
    T <- length(dat)
    alpha1 <- par[1]
    alpha2 <- par[2]
    pmf <- par[-(1:2)]
    pmf <- c(1 - sum(pmf), pmf)
    
    #Conditional likelihood:
    value <- 0
    for (t in c(3:T)) {
      cp <- 0
      for (i1 in c(0:min(dat[t], dat[t - 1]))) {
        cp <-
          cp + dbinom(i1, dat[t - 1], alpha1) * sum(dbinom((0:min(
            dat[t] - i1, dat[t - 2]
          )), dat[t - 2], alpha2) * pmf[dat[t] + 1 - i1 - (0:min(dat[t] - i1, dat[t-2]))])
      }
      value <- value - log(cp)
    }
    value
  })

sp_inar1 <- function(n, alpha, pmf, prerun = 500) {
  if(sum(pmf) != 1){warning("Sum of pmf entries has been standardized to 1.")}
  pmf <- pmf/sum(pmf)
  err <- sample(0:(length(pmf)-1), n + prerun, replace = TRUE, prob = pmf)
  x <- numeric(n + prerun)
  x[1] <- err[1]
  for (i in 2:(n + prerun)) {
    x[i] <- rbinom(1, x[i - 1], alpha) + err[i]
  }
  return(x[-seq_len(prerun)])
}

sp_inar2 <- function(n, alpha1, alpha2, pmf, prerun = 500) {
  if(sum(pmf) != 1){warning("Sum of pmf entries has been standardized to 1.")}
  pmf <- pmf/sum(pmf)
  err <- sample(0:(length(pmf)-1), n + prerun, replace = TRUE, prob = pmf)
  x <- numeric(n + prerun)
  x[1] <- err[1]
  x[2] <- x[1] + err[2]
  for (i in 3:(n + prerun)) {
    x[i] <-
      rbinom(1, x[i - 1], alpha1) + rbinom(1, x[i - 2], alpha2) + err[i]
  }
  return(x[-seq_len(prerun)])
}


dat <- sp_inar1(1000, 0.3, dpois(0:10,1.5))
spinar_boot(dat, 1, 10)

dat <- sp_inar2(500, 0.3, 0.2, dpois(0:10,1.5))
spinar_boot(dat, 2, 10)




