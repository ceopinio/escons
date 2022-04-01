#' Allocate seats to votes using D'Hondt formula
#'
#' @param votes A vector of total votes
#' @param seats An integer with the total number of seats to allocate
#' @return A vector of integers of the same as length \code{votes}
#'     with the number of seats corresponding to each party
#'
#' @note The function does not handle ties and relies on the internal
#'     \code{order} function
dhondt <- function(votes, seats, threshold=0.03) {
    divisors <- 1:seats
    quotas <- outer(votes, divisors, '/') ## How to deal with ties
    last_seat <- quotas[order(quotas, decreasing=TRUE)]
    receive_seat <- quotas >= last_seat[seats]
    seats <- rowSums(receive_seat)
    return(seats)
}

#' Allocate seats to parties according to their shares
#'
#' @param total_votes An integer with the total number of votes
#' @param shares A vector with the vote share for each shares2seats
#' @param threshold The proportion (between 0 and 1) of votes needed
#'     to be allocated a seat
#' @return A vector of length \code{length(shares)} with the number of
#'     seats allocated to each party
#'
#' @export
share2seats <- function(shares, seats, threshold=0.03) {
    shares_above_thr <- shares > threshold
    parties_above_thr <- which(shares_above_thr)
    valid_shares_above_thr <- shares[shares_above_thr]
    allocation <- rep(0, length(shares))
    allocation[parties_above_thr] <- dhondt(valid_shares_above_thr, seats)
    return(allocation)
}

#' Simulate vote shares for different parties
#'
#' Simulates vote shares extracted from a Dirichlet distribution with
#' parameters given by the vote share of each party
#' 
#' @param N An integer with the number of simulations
#' @param p A vector of proportions (between 0 and 1) with the vote
#'     share for each party
#' @return A matrix of dimensions \code{length(p)} x N with the
#'     simulated vote shares
#' @importFrom DirichletReg rdirichlet
simulate_vote_share <- function(p, N, ...) {
    if (!all.equal(sum(p), 1)) {
        stop("Vote shares do not add up to 1", call.=FALSE)
    }
    return(rdirichlet(N, p))
}

#' Simulate seat allocations
#'
#' @param seats An integer with the total number of seats to be
#'     allocated
#' @param p A vector of proportions (between 0 and 1) with the vote
#'     share for each party
#' @param N An integer with the number of simulations. Default is
#'     1,000
#' @param threshold A proportion (between 0 and 1) with the threshold
#' @return A list with two elements: \code{shares} is a matrix of size
#'     \code{length(p)} x \code{N} with the simulated vote share, and
#'     \code{seats} of the same dimensions with the simulated seat
#'     distribution
#' 
#' @export
simulate <- function(seats, p, N=1000, threshold=0.03, ...) {
    simulated_vote_shares <- simulate_vote_share(p, N)
    res <- apply(simulated_vote_shares,
                 1,
                 function(x) share2seats(x, seats, threshold),
                 simplify=FALSE)
    sims <- do.call(rbind, res)
    return(list("seats"=sims,
                "shares"=simulated_vote_shares))
}
