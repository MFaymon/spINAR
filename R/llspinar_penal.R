llspinar_penal <- list(
  function(par, dat, penal1, penal2) {
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
      nb <- penal1 * sum(abs(pmf[2:length(pmf)]-pmf[1:(length(pmf)-1)])) + 
        penal2 * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
      value <- value - (log(cp)-nb)
    }
    value
  },
  function(par, dat, penal1, penal2) {
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
      nb <- penal1 * sum(abs(pmf[2:length(pmf)]-pmf[1:(length(pmf)-1)])) + 
        penal2 * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
      value <- value - (log(cp)-nb)
    }
    value
  })
