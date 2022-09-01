#' @title Spinar(p)
#' @description This function takes a data set and the lag of INAR process
#' @param dat dataset
#' @param p lag
#'
#' @return Return the estimated parameters of the INAR(p). The first p-values
#' corresponds to (alpha_1,..., alpha_p)
#' the next values corresponds to the probability mass function (pmf0, pmf1, ...)
#' @export a vector value
#'
#' @examples spinar(c(2,3,1,1,1,1,1,1,3), 1)
#'2.362001e-11 4.205347e-09 6.128583e-01 5.021233e-10 3.871417e-01
#'
#importFrom("stats", "acf", "constrOptim", "dbinom")
spinar <- function(dat, p) {
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
  # constraints for input
  if (!is.integer(p)) {
    stop('Lag p for INAR(p) must be integer')
  }
  if (!is.vector(dat)) {
    stop('Data should be a vector')
  }
  if (length(dat) < 2) {
    stop('Data should be a vector with at least two entries')
  }
  if (!sum(as.integer(dat) == (dat)) == length(dat)) {
    stop('Data should be a vector with integer values')
  }
  # auxiliar function 1
  if (p == 1) {
    llspinar <- function(par, dat) {
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
    eacf1 <-
      ifelse(acf(dat, plot = FALSE)$acf[2] <= 0,
             0.00001,
             acf(dat, plot = FALSE)$acf[2])
    xmax <- max(dat)
    est <-
      suppressWarnings(constrOptim(
        c(eacf1, rep(1 / (xmax + 1), xmax)),
        llspinar,
        NULL,
        ui = constrmat(p, xmax),
        ci = constrvec(p, xmax),
        dat = dat
      ))
  }
  else if (p == 2) {
    llspinar <- function(par, dat) {
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
            )), dat[t - 2], alpha2) * pmf[dat[t] + 1 - i1 - (0:min(dat[t] - i1, dat[t -
                                                                                      2]))])
        }
        value <- value - log(cp)
      }

      value
    }
    eacf1 <-
      ifelse(acf(dat, plot = FALSE)$acf[2] <= 0,
             0.00001,
             acf(dat, plot = FALSE)$acf[2])
    eacf2 <-
      ifelse(acf(dat, plot = FALSE)$acf[3] <= 0,
             0.00001,
             acf(dat, plot = FALSE)$acf[3])
    xmax <- max(dat)
    est <-
      suppressWarnings(constrOptim(
        c(eacf1, eacf2, rep(1 / (xmax + 1), xmax)),
        llspinar,
        NULL,
        ui = constrmat(p, xmax),
        ci = constrvec(p, xmax),
        dat = dat
      ))
  }
  parameters <- est$par
  if (p==1) {
    parameters <- c(parameters[1], 1-sum(parameters[-1]), parameters[-1])
  }
  else if (p==2){
    parameters <- c(parameters[1:2], 1-sum(parameters[-(1:2)]), parameters[-(1:2)])
  }
  return(parameters)
}




