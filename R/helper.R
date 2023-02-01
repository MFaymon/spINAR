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

.makeProgressBar = function(progress, ...) {
  if (progress && getOption("width") >= 20L){
    progress_bar$new(...)
  }else{
    list(tick = function(len = 1, tokens = list()) NULL, update = function(ratio, tokens) NULL)
  }
}
