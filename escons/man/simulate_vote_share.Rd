% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.R
\name{simulate_vote_share}
\alias{simulate_vote_share}
\title{Simulate vote shares for different parties}
\usage{
simulate_vote_share(shares, sigmas, N, ...)
}
\arguments{
\item{shares}{A vector of vote shares for each party}

\item{sigmas}{A vector of vote share uncertainty for ech party}

\item{N}{An integer with the number of simulations}

\item{...}{Additional arguments (currently not used)}
}
\value{
A matrix of dimensions \code{length(p)} x N with the
simulated vote shares
}
\description{
Simulates vote shares extracted from a normal distribution centered
at \code{shares} and with standard deviation \code{sigma}
}
