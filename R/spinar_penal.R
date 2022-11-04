#' Penalized (semiparametric) estimation of INAR(p) model.
#'
#' @param x : data
#' @param p : parameter of the INAR(p)
#' @param penal1 : default value penal1=0 meaning no penalization.
#' @param penal2 : default value penal2=0 meaning no penalization.
#'
#' @return parameters
#' @export
#'
#' @examples
spinar_penal <- function(x, p, penal1=0, penal2=0) {
  # constraints for input
  checkmate::assert_integerish(p, lower = 1, min.len = 1, max.len = 1, upper = 2)
  checkmate::assert_integerish(x, min.len = p+1)
  checkmate::assert_numeric(penal1, len = 1)
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
      f = llspinar_penal[[p]],
      grad = NULL,
      ui = .constrmat(p, xmax),
      ci = .constrvec(p, xmax),
      dat = x,
      penal1 = penal1,
      penal2 = penal2
    ))
    parameters <- est$par
    parameters <- c(parameters[seq_len(p)],
                    1-sum(parameters[-seq_len(p)]),
                    parameters[-seq_len(p)])
  }
  return(parameters)
}

# small running examples

n <- 100
alpha <- 0.5

dat <- sp_inar1(n, alpha, dpois(0:20,1))

set.seed(199)
unpenal <- spinar(dat,1)
set.seed(199)
penal00 <- spinar_penal(dat,1,0,0)
set.seed(199)
penal10 <- spinar_penal(dat,1,1,0)
set.seed(199)
penal01 <- spinar_penal(dat,1,0,1)
set.seed(199)
penalhh <- spinar_penal(dat,1,0.5,0.5)

par(mfrow=c(3,2))
barplot(penal00[-1], main="no penalization", ylim=c(0,0.5))
barplot(penal10[-1], main="only L1", ylim=c(0,0.5))
barplot(penal01[-1], main="only L2", ylim=c(0,0.5))
barplot(penalhh[-1], main="L1 and L2", ylim=c(0,0.5))
barplot(dpois(0:6,1), main="truth", ylim=c(0,0.5))

n <- 1000
alpha <- 0.5

dat <- sp_inar1(n, alpha, dpois(0:20,1))

set.seed(199)
unpenal <- spinar(dat,1)
set.seed(199)
penal00 <- spinar_penal(dat,1,0,0)
set.seed(199)
penal10 <- spinar_penal(dat,1,1,0)
set.seed(199)
penal01 <- spinar_penal(dat,1,0,1)
set.seed(199)
penalhh <- spinar_penal(dat,1,0.5,0.5)

par(mfrow=c(3,2))
barplot(penal00[-1], main="no penalization", ylim=c(0,0.5))
barplot(penal10[-1], main="only L1", ylim=c(0,0.5))
barplot(penal01[-1], main="only L2", ylim=c(0,0.5))
barplot(penalhh[-1], main="L1 and L2", ylim=c(0,0.5))
barplot(dpois(0:6,1), main="truth", ylim=c(0,0.5))


