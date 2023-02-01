#' @title Validated penalized semiparametric estimation of INAR models
#'
#' @description
#' Semiparametric penalized estimation of the autoregressive parameters and the innovation distribution of INAR(\code{p}) models,
#' \eqn{\code{p} \in \{1,2\}}. The estimation is conducted by maximizing the penalized conditional likelihood of the model.
#' Included is a possible validation of one or both penalization parameters. If no validation is wanted, the function coincides
#' to the spinar_penal function of this package.
#'
#' @param x [\code{integer}]\cr
#' vector with integer observations.
#' @param p [\code{integer(1)}]\cr
#' order of the INAR model, where \eqn{\code{p} \in \{1,2\}}.
#' @param validation [\code{logical(1)}]\cr
#' indicates whether validation is wanted.
#' @param penal1 [\code{numeric(1)}]\cr
#' \eqn{L_1} penalization parameter.
#' It will be ignored if \code{validation = TRUE} and \code{over} \eqn{\in \{"both", "L_1"\}}. It is mandatory if \code{validation = FALSE}.
#' @param penal2 [\code{numeric(1)}]\cr
#' \eqn{L_2} penalization parameter.
#' It will be ignored if \code{validation = TRUE} and \code{over} \eqn{\in \{"both", "L_2"\}}. It is mandatory if \code{validation = FALSE}.
#' @param over [\code{string(1)}]\cr
#' validation over \code{"both"} penalization parameters or only over \code{"L_1"} or \code{"L_2"}.
#' It is mandatory if \code{validation = TRUE}, otherwise it will be ignored.
#' @param folds [\code{integer(1)}]\cr
#' number of folds for (cross) validation.
#' @param init1 [\code{numeric(1)}]\cr
#' initial value for penal1 in validation. Default value is init1 = 1.
#' @param init2 [\code{numeric(1)}]\cr
#' initial value for penal2 in validation. Default value is init2 = 1
#'
#' @return For \code{validation=FALSE}, the function returns a vector containing the penalized estimated coefficients
#' \eqn{\code{alpha}_1,...,\code{alpha}_p} and the penalized estimated entries of the pmf \eqn{\code{pmf}_0, \code{pmf}_1}... where \eqn{\code{pmf}_i} represents the probability of
#' an innovation being equal to \eqn{i}. For \code{validation=TRUE}, the function returns a named list, where the first entry contains
#' the penalized estimated coefficients \eqn{\code{alpha}_1,...,\code{alpha}_p} and the penalized estimated entries of the pmf \eqn{\code{pmf}_0, \code{pmf}_1},... where \eqn{\code{pmf}_i} represents the probability of
#' an innovation being equal to \eqn{i}. The second (and if \code{over = both} also the third entry) contain(s) the validated penalization paramter(s).
#'
#' @examples
#' # generate data
#' dat1 <- spinar_sim(n = 50, p = 1, alpha = 0.5,
#'                    pmf = c(0.3, 0.3, 0.2, 0.1, 0.1))
#'
#' \dontrun{
#' # penalized semiparametric estimation with validation over L1
#' spinar_penal_val(x = dat1, p = 1, validation = TRUE, penal2 = 0.1,
#'                  over = "L1")
#' # penalized semiparametric estimation with validation over both L1 and L2
#' spinar_penal_val(x = dat1, p = 1, validation = TRUE, over = "both")}
#'
#' @export spinar_penal_val
spinar_penal_val <- function(x, p, validation, penal1 = NA, penal2 = NA, over = NA, folds = 10, init1 = 1, init2 = 1){
  assert_integerish(p, lower = 1, len = 1, upper = 2)
  assert_integerish(x, lower = 0, min.len = p+1)
  assert_numeric(penal1, len = 1)
  assert_numeric(penal2, len = 1)

  if(validation == FALSE){
    if(is.na(penal1) || is.na(penal2)){warning("values for penal1 or penal2 are missing, they are therefore treated as zero")}
    if(is.na(penal1)){penal1 <- 0}
    if(is.na(penal2)){penal2 <- 0}
    parameters <- spinar_penal(x, p, penal1, penal2)
  } else{
    assert_logical(validation)
    assert(checkChoice(over, c("L1", "L2", "both")))
    assert_integerish(folds, lower = 2, upper = floor((length(x)/(p+1))), len = 1)
    assert_numeric(init1, len = 1)
    assert_numeric(init2, len = 1)

    n <- length(x)

    ins <- vector(mode = "list", folds)
    outs <- vector(mode = "list", folds)

    if(n%%folds==0){
      for(d in 1:folds){
        ins[[d]] <- x[-((1+(d-1)*(n/folds)):((n/folds)+(d-1)*(n/folds)))]
        outs[[d]] <- x[(1+(d-1)*(n/folds)):((n/folds)+(d-1)*(n/folds))]
      }
    } else{
      lngth <- rep(n %/% folds, folds)
      for(i in 1:(n %% folds)){
        lngth[i] <- lngth[i] + 1
      }

      idx <- list()
      idx[[1]] <- 1:lngth[1]
      for(i in 2:length(lngth)){
        idx[[i]] <- sum(lngth[1:(i-1)],1):sum(lngth[1:i])
      }

      for(d in 1:folds){
        ins[[d]] <- x[-idx[[d]]]
        outs[[d]] <- x[idx[[d]]]
      }
    }

    if(over == "L1"){
      if(!is.na(penal1)){stop("if over = L1, no value for penal1 allowed")}
      if(is.na(penal2)){warning("value for penal2 is missing and is treated as zero")}
      if(is.na(penal2)){penal2 <- 0}
      penal_val <- init1

      loglik <- matrix(NA, folds, 5)

      repeat{
        penal_vals <- c(penal_val-0.1, penal_val-0.05, penal_val, penal_val+0.05, penal_val+0.1)

        for(f in 1:folds){
          data_in <- ins[[f]]
          data_out <- outs[[f]]

          for (l in 1:length(penal_vals)){
            penal <- penal_vals[l]
            est <- spinar_penal(x, p, penal, penal2)
            alpha <- est[seq_len(p)]
            pmf <- est[-seq_len(p)]

            value <- 0

            if(p == 1){
              for(t in 2:length(data_out)){
                cp <- sum(dbinom(0:min(data_out[t],data_out[t-1]), data_out[t-1], alpha) * pmf[data_out[t]+1-(0:min(data_out[t],data_out[t-1]))])
                nb <- penal * sum(abs(pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])) + penal2 * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value-(log(cp)-nb)
              }
            }

            if(p ==2){
              alpha1 <- alpha[1]
              alpha2 <- alpha[2]
              for (t in 3:length(data_out)){
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

        if(length(index) > 1){
          if(3 %in% index){
            penal1_opt <- penal_val
            break
          } else{
            index <- min(index)
          }
        }

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
      if(!is.na(penal2)){stop("if over = L2, no value for penal2 allowed")}
      if(is.na(penal1)){warning("value for penal1 is missing and is treated as zero")}
      if(is.na(penal1)){penal1 <- 0}
      penal_val <- init2

      loglik <- matrix(NA, folds, 5)

      repeat{
        penal_vals <- c(penal_val-0.1, penal_val-0.05, penal_val, penal_val+0.05, penal_val+0.1)

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
              for(t in 2:length(data_out)){
                cp <- sum(dbinom(0:min(data_out[t],data_out[t-1]), data_out[t-1], alpha) * pmf[data_out[t]+1-(0:min(data_out[t],data_out[t-1]))])
                nb <- penal1 * sum(abs(pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])) + penal * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value-(log(cp)-nb)
              }
            }

            if(p ==2){
              alpha1 <- alpha[1]
              alpha2 <- alpha[2]
              for (t in 3:length(data_out)){
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

        if(length(index) > 1){
          if(3 %in% index){
            penal2_opt <- penal_val
            break
          } else{
            index <- min(index)
          }
        }

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
      if(!is.na(penal1) || !is.na(penal2)){warning("if over = both, input values for penal1 and penal2 are ignored")}

      penal_val1 <- init1
      penal_val2 <- init2

      loglik <- matrix(NA, folds, 9)

      repeat{
        penal_vals1 <- c(penal_val1-0.05, penal_val1, penal_val1+0.05)
        penal_vals2 <- c(penal_val2-0.05, penal_val2, penal_val2+0.05)
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
              for(t in 2:length(data_out)){
                cp <- sum(dbinom(0:min(data_out[t],data_out[t-1]), data_out[t-1], alpha) * pmf[data_out[t]+1-(0:min(data_out[t],data_out[t-1]))])
                nb <- penal1 * sum(abs(pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])) + penal2 * sum((pmf[2:length(pmf)]-pmf[0:(length(pmf)-1)])^2)
                value <- value-(log(cp)-nb)
              }
            }

            if(p ==2){
              alpha1 <- alpha[1]
              alpha2 <- alpha[2]
              for (t in 3:length(data_out)){
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

        if(length(index) > 1){
          if(5 %in% index){
            penal1_opt <- penal_val1
            penal2_opt <- penal_val2
            break
          } else{
            index <- min(index)
          }
        }

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
