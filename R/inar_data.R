##################################################################################################################
# Function to generate INAR(p) data for p \in {1,2}
# We allow for general pmf's which can be generated parametrically or "manually" (semiparametric)
##################################################################################################################

## INAR(1) 

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

# small examples

n <- 1000
alpha <- 0.5

sp_inar1(n, alpha, dpois(0:20,1))
sp_inar1(n, alpha, c(0.1,0.5,0.4))
sp_inar1(n, alpha, c(0.1,0.5,0.2)) #warning

## INAR(2)

# small exmaples

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

n <- 1000
alpha1 <- 0.2
alpha2 <- 0.3

sp_inar2(n, alpha1, alpha2, dpois(0:20,1))
sp_inar2(n, alpha1, alpha2, c(0.1,0.5,0.4))
sp_inar2(n, alpha1, alpha2, c(0.1,0.5,0.2)) #warning



