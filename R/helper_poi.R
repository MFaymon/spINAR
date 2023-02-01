.constrmat_poi <- function(p) {
  mat <- array(0, c(p + 2, p + 1))
  mat[1:(p + 1), 1:(p + 1)] <- diag(1, p + 1)
  mat[p + 2, 1:p] <- (-1)
  mat
}

.constrvec_poi <- function(p) {
  vec <- rep(0, p + 2)
  vec[p + 2] <- (-1)
  vec
}
