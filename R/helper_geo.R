.constrmat_geo <- function(p) {
  mat <- array(0, c(p + 3, p + 1))
  mat[1:(p + 1), 1:(p + 1)] <- diag(1, p + 1)
  mat[p + 2, 1:p] <- (-1)
  mat[p + 3, p + 1] <- (-1)
  mat
}

.constrvec_geo <- function(p) {
  vec <- rep(0, p + 3)
  vec[(p + 2):(p + 3)] <- (-1)
  vec
}
