.constrmat_nb <- function(p) {
  mat <- array(0, c(p + 4, p + 2))
  mat[1:(p + 2), 1:(p + 2)] <- diag(1, p + 2)
  mat[p + 3, 1:p] <- (-1)
  mat[p + 4, p + 2] <- (-1)
  mat
}

.constrvec_nb <- function(p) {
  vec <- rep(0, p + 4)
  vec[(p + 3):(p + 4)] <- (-1)
  vec
}
