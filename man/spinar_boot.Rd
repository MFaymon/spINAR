% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inar_sp_boot.R
\name{spinar_boot}
\alias{spinar_boot}
\title{Semiparametric Integer Autoregressive Bootstrap Procedure}
\usage{
spinar_boot(x, p, B)
}
\arguments{
\item{x}{[\code{integer}]\cr
vector of integer values corresponding to the data}

\item{p}{[\code{integer(1)}]\cr
order of the INAR model, where \code{p in \{1,2\}}}

\item{B}{[\code{integer(1)}]\cr
number of bootstrap repetitions}
}
\value{
bootstrap estimated parameters (INAR coefficients and pmf)
}
\description{

}
\examples{
### simulating data of INAR(1)
# dat <- spinar_sim(n=1000, p = 1, alpha = 0.3, pmf = dpois(0:10,1.5))
### semiparametric INAR bootstrap
# spinar_boot(dat, 1, 100)
### simulating data of INAR(2)
# dat <- spinar_sim(n=500, p = 2, alpha = c(0.3, 0.2), pmf = dpois(0:10,1.5))
### semiparametric INAR bootstrap
# spinar_boot(dat, 2, 100)
}
