#' @title Validation of the Penalization Parameters of the Penalized Semiparametric Estimation of INAR(p) Model
#'
#' @description
#' Performs a validation of one or both penalization parameters of the penalized semiparametric estimation of INAR(p) models,
#' \code{p in {1,2}}. If no validation is wanted, the function coincides to the spinar_penal function of this package.
#'
#' @param x [\code{integer}]\cr
#' vector of integer values corresponding to the data
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \code{p in \{1,2\}}
#' @param validation [\code{logical(1)}]\cr
#' `TRUE` or `FALSE` depending on whether validation is wanted
#' @param penal1 [\code{numeric(1)}]\cr
#' penalization parameter for `L1` penalization
#' It will be ignored if validation = `TRUE` and over = `both`. Also, `penal1` will be ignored if validation = `TRUE` and over = `L1`.
#' It is mandatory if validation = `FALSE`.
#' @param penal2 [\code{numeric(1)}]\cr
#' penalization parameter for L2 penalization
#' It will be ignored if validation = `TRUE` and over = `both`. Also `penal2` will be ignored if validation = `TRUE` and over = `L2`.
#' It is mandatory if validation = `FALSE`.
#' @param over [\code{string(1)}]\cr
#' can take answers whether validation for penal1 (`L1`) or penal2 (`L2`) or both (`both`) is wanted.
#' It is mandatory if validation = `TRUE`, otherwise it will be ignored.
#' @param folds [\code{integer(1)}]\cr
#' number of folds for cross validation
#' @param init1 [\code{numeric(1)}]\cr
#' initial value for penal1 in validation. Default value is init1 = 1.
#' @param init2 [\code{numeric(1)}]\cr
#' initial value for penal2 in validation. Default value is init2 = 1
#'
#' @return estimated parameters \code{(alpha_1, ..., alpha_p, pmf[0], pmf[1], ...)},
#' where \code{(alpha_1, ..., alpha_p)} are the estimated autoregressive coefficients
#' and \code{(pmf[0], pmf[1], ...)} are the estimated entries of the probability mass function of the innovation distribution,
#' where \code{pmf[i]} denotes the probability of observing value i
#'
#' @export
#'
#' @examples
#' ### data generation
#' # do not run
#' # dat <- spinar_sim(1000, 1, 0.5, dpois(0:20,1))
#' ### penalized semiparametric estimation with validation over both penalization parameters
#' # spinar_penal_val(dat, 1, validation=TRUE, over="both")
#' #### data generation
#' # dat <- spinar_sim(1000, 1, 0.5, dpois(0:20,1))
#' ## penalized semiparametric estimation with validation over L1 penalization parameter
#' # spinar_penal_val(dat, 1, validation=TRUE, penal2 = 0.1, over="L1")

spinar_penal_val <- function(x, p, validation, penal1=NA, penal2=NA, over=NA, folds = 10, init1 = 1, init2 = 1){
  # also allow for window?: length of window around penal values -> ???
  # if we only want to have one function for (semiparametric) estimation (penalized and unpenalized), we should write in the
  # documentation that unpenalized estimation is performed for validation = FALSE and no values set for penal1 and
  # penal2 (default = 0) or if the user set them explicitly = 0.
  # maybe keep the function for unpenalized estimation, makes it easier for the user

  # cite our paper to explain the penalization and the validation (Algorithm 1)? Or explain it shortly in the documentation
  # (and additionally cite the paper?)?

  checkmate::assert_integerish(p, lower = 1, len = 1, upper = 2)
  checkmate::assert_integerish(x, lower = 0, min.len = p+1)
  checkmate::assert_numeric(penal1, len = 1)
  checkmate::assert_numeric(penal2, len = 1)

  if(validation == FALSE){
    # if validation = FALSE, the user has to input values for penal1 and penal2
    # issue a warning if no values for pena1 and penal2 are set -> function takes default values (both zero)
    #if(missing(penal1) || missing(penal2)){warning("values for penal1 or penal2 are missing, they are therefore treated as zero")}
    if(is.na(penal1) || is.na(penal2)){warning("values for penal1 or penal2 are missing, they are therefore treated as zero")}
    parameters <- spinar_penal(x, p)
  } else{

    checkmate::assert_logical(validation) #if(!is.logical(validation)){stop("'validation' has to be logical")} #translate to checkmate
    checkmate::assert(checkmate::checkChoice(over, c("L1", "L2", "both", NA))) # additionally ensure that over is either 'L1', 'L2' or 'both'
    checkmate::assert_integerish(folds, upper = ceiling((length(x)/2)), len = 1) # issue error message if n/folds < 2 if not: not enough observations for validation
    checkmate::assert_numeric(init1, len = 1)
    checkmate::assert_numeric(init2, len = 1)


    # separate the data in in and out of sample data
    n <- length(x) # issue error message if n/folds < 2 if not: not enough observations for validation
    folds <- 10

    ins <- vector(mode = "list", folds)
    outs <- vector(mode="list", folds)

    if(n%%folds==0){ #if rest of division is zero
      for(d in 1:folds){
        ins[[d]] <- x[-((1+(d-1)*(n/folds)):((n/folds)+(d-1)*(n/folds)))] #in sample data
        outs[[d]] <- x[(1+(d-1)*(n/folds)):((n/folds)+(d-1)*(n/folds))] #out of sample data
      }
    } else{ #if rest of division is unequal zer0
      lngth <- rep(n %/% folds, folds)
      for(i in 1:(n %% folds)){
        lngth[i] <- lngth[i] + 1
      }

      index <- list()
      index[[1]] <- 1:lngth[1]
      for(i in 2:length(lngth)){
        index[[i]] <- sum(lngth[1:(i-1)],1):sum(lngth[1:i])
      }

      # out ist das Kuerzere

      for(d in 1:folds){
        ins[[d]] <- x[-index[[d]]] #in sample data
        outs[[d]] <- x[index[[d]]] #out of sample data
      }
    }

    # if validation = TRUE, input values for penal1 and penal2 will be ignored for over = both and partial for over = L1 or L2 resp.
    # issue a warning about this
    if(over == "L1"){
      # only validation for penal1, the value for penal2 has to be input by the user
      # warning: if not then treated as zero
      #if(!missing(penal1)){stop("if over = L1, no value for penal1 allowed")}
      if(!is.na(penal1)){stop("if over = L1, no value for penal1 allowed")}
      #if(missing(penal2)){warning("value for penal2 is missing and is treated as zero")}
      if(is.na(penal2)){warning("value for penal2 is missing and is treated as zero")}
      #if(missing(penal2)){penal2 <- 0}
      if(is.na(penal2)){penal2 <- 0}
      penal_val <- init1

      loglik <- matrix(NA, folds, 5) # 5 is length of lambda grid

      repeat{
        penal_vals <- c(penal_val-0.1, penal_val-0.05, penal_val, penal_val+0.05, penal_val+0.1) # lambda grid

        for(f in 1:folds){
          data_in <- ins[[f]]
          data_out <- outs[[f]]

          for (l in 1:length(penal_vals)){
            penal <- penal_vals[l]
            est <- spinar_penal(x, p, penal, penal2)
            alpha <- est[seq_len(p)]
            pmf <- est[-seq_len(p)]

            value <- 0

            if(p ==1){
              for(t in 2:length(data_out)){ #compute log-likelihood
                cp <- sum(dbinom(0:min(data_out[t],data_out[t-1]), data_out[t-1], alpha) * pmf[data_out[t]+1-(0:min(data_out[t],data_out[t-1]))])
                nb <- penal * sum(abs(pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])) + penal2 * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value-(log(cp)-nb)
              }
            }

            if(p ==2){
              alpha1 <- alpha[1]
              alpha2 <- alpha[2]
              for (t in 3:length(data_out)){ #compute log-likelihood
                cp <- 0
                for (i1 in c(0:min(data_out[t], data_out[t - 1]))) {
                  cp <- cp + dbinom(i1, data_out[t - 1], alpha1) * sum(dbinom((0:min(data_out[t] - i1, data_out[t - 2])), data_out[t - 2], alpha2)
                                                                       * pmf[data_out[t] + 1 - i1 - (0:min(data_out[t] - i1, data_out[t-2]))])
                }
                nb <- penal * sum(abs(pmf[2:length(pmf)]-pmf[1:(length(pmf)-1)])) +
                  penal2 * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value - (log(cp)-nb)
              }
            }

            loglik[f,l] <- value
          }

        }

        aaf <- apply(loglik, 2, function(x) mean(x, na.rm=TRUE))
        index <- which(aaf==min(aaf))
        penal1_opt <- penal_vals[index]

        if(penal1_opt < 0){
          penal1_opt <- 0
          break
        }

        if(penal1_opt==penal_val){
          break
        }

        penal_val <- penal1_opt
      }


      parameters <- spinar_penal(x, p, penal1_opt, penal2)

    }
    if(over == "L2"){
      # only validation for penal2, the value for penal1 has to be input by the user
      # warning: if not then treated as zero
      #if(!missing(penal2)){stop("if over = L2, no value for penal2 allowed")}
      #if(missing(penal1)){warning("value for penal1 is missing and is treated as zero")}
      #if(missing(penal1)){penal1 <- 0}
      if(!is.na(penal2)){stop("if over = L2, no value for penal2 allowed")}
      if(is.na(penal1)){warning("value for penal1 is missing and is treated as zero")}
      if(is.na(penal1)){penal1 <- 0}
      penal_val <- init2

      loglik <- matrix(NA, folds, 5) # 5 is length of lambda grid

      repeat{
        penal_vals <- c(penal_val-0.1, penal_val-0.05, penal_val, penal_val+0.05, penal_val+0.1) # lambda grid

        for(f in 1:folds){
          data_in <- ins[[f]]
          data_out <- outs[[f]]

          for (l in 1:length(penal_vals)){
            penal <- penal_vals[l]
            est <- spinar_penal(x, p, penal1, penal)
            alpha <- est[seq_len(p)]
            pmf <- est[-seq_len(p)]

            value <- 0

            if(p ==1){
              for(t in 2:length(data_out)){ #compute log-likelihood
                cp <- sum(dbinom(0:min(data_out[t],data_out[t-1]), data_out[t-1], alpha) * pmf[data_out[t]+1-(0:min(data_out[t],data_out[t-1]))])
                nb <- penal1 * sum(abs(pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])) + penal * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value-(log(cp)-nb)
              }
            }

            if(p ==2){
              alpha1 <- alpha[1]
              alpha2 <- alpha[2]
              for (t in 3:length(data_out)){ #compute log-likelihood
                cp <- 0
                for (i1 in c(0:min(data_out[t], data_out[t - 1]))) {
                  cp <- cp + dbinom(i1, data_out[t - 1], alpha1) * sum(dbinom((0:min(data_out[t] - i1, data_out[t - 2])), data_out[t - 2], alpha2)
                                                                       * pmf[data_out[t] + 1 - i1 - (0:min(data_out[t] - i1, data_out[t-2]))])
                }
                nb <- penal1 * sum(abs(pmf[2:length(pmf)]-pmf[1:(length(pmf)-1)])) +
                  penal * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value - (log(cp)-nb)
              }
            }

            loglik[f,l] <- value
          }

        }

        aaf <- apply(loglik, 2, function(x) mean(x, na.rm=TRUE))
        index <- which(aaf==min(aaf))
        penal2_opt <- penal_vals[index]

        if(penal2_opt < 0){
          penal2_opt <- 0
          break
        }

        if(penal2_opt==penal_val){
          break
        }

        penal_val <- penal2_opt
      }


      parameters <- spinar_penal(x, p, penal1, penal2_opt)

    }
    if(over == "both"){
      #if(!missing(penal1) || !missing(penal2)){warning("if over = both, input values for penal1 and penal2 are ignored")}
      if(!is.na(penal1) || !is.na(penal2)){warning("if over = both, input values for penal1 and penal2 are ignored")}
      # validation for both penal1 and penal2

      penal_val1 <- init1
      penal_val2 <- init2

      loglik <- matrix(NA, folds, 9) # 9 is length of combined lambda grid (3 for each)

      repeat{
        penal_vals1 <- c(penal_val1-0.05, penal_val1, penal_val1+0.05) # lambda grid
        penal_vals2 <- c(penal_val2-0.05, penal_val2, penal_val2+0.05) # lambda grid
        grid <- expand.grid(penal_vals1, penal_vals2)

        for(f in 1:folds){
          data_in <- ins[[f]]
          data_out <- outs[[f]]

          for (l in 1:nrow(grid)){
            penal1 <- grid[l,1]
            penal2 <- grid[l,2]
            est <- spinar_penal(x, p, penal1, penal2)
            alpha <- est[seq_len(p)]
            pmf <- est[-seq_len(p)]

            value <- 0

            if(p ==1){
              for(t in 2:length(data_out)){ #compute log-likelihood
                cp <- sum(dbinom(0:min(data_out[t],data_out[t-1]), data_out[t-1], alpha) * pmf[data_out[t]+1-(0:min(data_out[t],data_out[t-1]))])
                nb <- penal1 * sum(abs(pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])) + penal2 * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value-(log(cp)-nb)
              }
            }

            if(p ==2){
              alpha1 <- alpha[1]
              alpha2 <- alpha[2]
              for (t in 3:length(data_out)){ #compute log-likelihood
                cp <- 0
                for (i1 in c(0:min(data_out[t], data_out[t - 1]))) {
                  cp <- cp + dbinom(i1, data_out[t - 1], alpha1) * sum(dbinom((0:min(data_out[t] - i1, data_out[t - 2])), data_out[t - 2], alpha2)
                                                                       * pmf[data_out[t] + 1 - i1 - (0:min(data_out[t] - i1, data_out[t-2]))])
                }
                nb <- penal1 * sum(abs(pmf[2:length(pmf)]-pmf[1:(length(pmf)-1)])) +
                  penal2 * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value - (log(cp)-nb)
              }
            }

            loglik[f,l] <- value
          }

        }

        aaf <- apply(loglik, 2, function(x) mean(x, na.rm=TRUE))
        index <- which(aaf==min(aaf))
        penal1_opt <- grid[index,1]
        penal2_opt <- grid[index,2]

        if(penal1_opt < 0){
          penal1_opt <- 0
        }

        if(penal2_opt < 0){
          penal2_opt <- 0
        }

        if(penal1_opt==penal_val1 && penal2_opt==penal_val2){
          break
        }

        penal_val1 <- penal1_opt
        penal_val2 <- penal2_opt
      }


      parameters <- spinar_penal(x, p, penal1_opt, penal2_opt)
    }
  }
  if(validation == FALSE){
    return(parameters)
  }else{
    if(!exists("penal2_opt")){return(list("parameters"=parameters,"penal1_opt"=penal1_opt))}
    if(!exists("penal1_opt")){return(list("parameters"=parameters,"penal2_opt"=penal2_opt))}
    if(exists("penal1_opt") && exists("penal2_opt")){return(list("parameters"=parameters,"penal1_opt"=penal1_opt,"penal2_opt"=penal2_opt))}
  }
}
