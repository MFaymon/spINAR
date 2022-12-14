% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spinar_est_param.R
\name{spinar_est_param}
\alias{spinar_est_param}
\title{Parametric estimation of INAR models}
\usage{
spinar_est_param(x, p, type, distr)
}
\arguments{
\item{x}{[\code{integer}]\cr
vector of integer values corresponding to the data}

\item{p}{[\code{integer(1)}]\cr
order of the INAR model, where \code{p in \{1,2\}}}

\item{type}{[\code{string(1)}]\cr
type of estimation in \code{\{"mom", "ml"\}}
'mom': moment estimation
'ml' : maximum likelihood estimation}

\item{distr}{[\code{string(1)}]\cr
parametric family of distribution in  \code{\{'poi', 'geo', 'nb'\}}
\code{'poi'}: Poisson distribution with parameter \code{lambda}
\code{'geo'}: Geometric distribution with parameter \code{prob}
\code{'nb'}: Negative binomial distribution with parameters \code{r} and \code{prob}}
}
\value{
(alpha, parameters)
where parameters depends on the distribution
parameters of poi, geo and nb are lambda, prob and (r, prob) respectively.
}
\description{
for now only moment-based
}
\examples{
## Examples
# do not run
# geominar1 <- function(n, alpha, pr){
#   err <- rgeom(n, pr)
#   x <- numeric(n)
#   #initialization x_0 = 0
#   x[1] <- err[1]
#   for(i in 2:n){
#     x[i] <- rbinom(1, x[i-1], alpha) + err[i]
#   }
#   return(x)
# }
# geominar2 <- function(n, alpha1, alpha2, pr){
#   err <- rgeom(n, pr)
#   x <- numeric(n)
#   #initialization x_0 = 0
#   x[1] <- err[1]
#   x[2] <- x[1] + err[2]
#   for(i in 3:n){
#     x[i] <- rbinom(1, x[i-1], alpha1) + rbinom(1, x[i-2], alpha2) + err[i]
#   }
#   return(x)
# }
# nbinar1 <- function(n, alpha, nsize, pi){
#   err <- rnbinom(n, nsize, pi)
#   x <- numeric(n)
#   #initialize x_0 = 0
#   x[1] <- err[1]
#   for(i in 2:n){
#     x[i] <- rbinom(1, x[i-1], alpha) + err[i]
#   }
#   return(x)
# }
# nbinar2 <- function(n, alpha1, alpha2, nsize, pi){
#   err <- rnbinom(n, nsize, pi)
#   x <- numeric(n)
#   #initialize x_0 = 0
#   x[1] <- err[1]
#   x[2] <- x[1] + err[2]
#   for(i in 3:n){
#     x[i] <- rbinom(1, x[i-1], alpha1) + rbinom(1, x[i-2], alpha2) + err[i]
#   }
#   return(x)
# }
# pinar1 <- function(n, alpha, lambda){
#   err <- rpois(n, lambda)
#   x <- numeric(n)
#   #initialization x_0 = 0
#   x[1] <- err[1]
#   for(i in 2:n){
#     x[i] <- rbinom(1, x[i-1], alpha) + err[i]
#   }
#   return(x)
# }
# pinar2 <- function(n, alpha1, alpha2, lambda){
#   err <- rpois(n, lambda)
#   x <- numeric(n)
#   #initialization
#   x[1] <- err[1]
#   x[2] <- x[1] + err[2]
#   for(i in 3:n){
#     x[i] <- rbinom(1, x[i-1], alpha1) + rbinom(1, x[i-2], alpha2) + err[i]
#   }
#   return(x)
# }
dat1 <- pinar1(1000,0.3, 1.4)
# dat2 <- pinar2(1000,0.3, 0.5, 0.8)
# dat3 <- geominar1(1000,0.3, 0.5)
# dat4 <- geominar2(1000, 0.3, 0.15, 0.5)
# dat5 <- nbinar1(1000, 0.4, 5, 0.33)
# dat6 <- nbinar2(1000, 0.3, 0.5, 5, 0.16) #extremely high running time -> exclude?
# spinar_est_param(dat1,1,"mom","poi")
# spinar_est_param(dat1,1,"ml","poi")
# spinar_est_param(dat2,2,"mom","poi")
# spinar_est_param(dat2,2,"ml","poi")
# spinar_est_param(dat3,1,"mom","geo")
# spinar_est_param(dat3,1,"ml","geo")
# spinar_est_param(dat4,2,"mom","geo")
# spinar_est_param(dat4,2,"ml","geo")
# spinar_est_param(dat5,1,"mom","nb")
# spinar_est_param(dat5,1,"ml","nb")
# spinar_est_param(dat6,2,"mom","nb")
# spinar_est_param(dat6,2,"ml","nb")
}
