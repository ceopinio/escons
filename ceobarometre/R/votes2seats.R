#' Allocate seats to votes using D'Hondt formula
#'
#' @param seats An integer with the total number of seats to allocate.
#' @param votes A vector with the votes (integer or proportion) per
#'   parties.
#' @return A vector of integers of the same as length \code{votes}
#'   with the number of seats corresponding to each party
#'
#' @note The function handles ties by randomly selecting any of the
#'   parties with ties at a given step. As a result, the function may
#'   produce different results from the official ones.
dhondt <- function(seats, votes) {
  divisors <- 1:seats
  quotas <- outer(votes, divisors, "/")
  ## Matrix to hold the winner of each seat
  W <- matrix(FALSE, nrow=nrow(quotas), ncol=ncol(quotas)) 
  ties <- 0 ## Ties counter
  
  for (i in 1:ncol(quotas)) {
    candidates <- which(quotas == max(quotas, na.rm=TRUE), arr.ind=TRUE)
    ## If there are ties, select one of them at random
    if (nrow(candidates) > 1) {
      ties <- ties + 1
      candidates <- candidates[sample(1:nrow(candidates), 1), ]
    }
    candidates <- as.vector(candidates) ## Flatten
    W[candidates[1], candidates[2]] <- TRUE
    ## Remove quota from consideration
    quotas[candidates[1], candidates[2]] <- NA
  }
  
  seats <- rowSums(W)
  
  if (ties > 0) {
    warning(sprintf("%s ties were found during the seat allocation", ties), call.=FALSE)
  }
  
  return(seats)
}

#' Allocate seats to parties according to their shares
#'
#' @param seats An integer with the total number of seats
#' @param shares A vector with the vote share (between 0 and 1) for each party
#' @param threshold The electoral threshold (between 0 and 1)
#' @return A vector of length \code{length(shares)} with the number of
#'   seats allocated to each party
#'
#' @examples
#' \dontrun{
#' res2021 <- c(25.0, 20.4, 17.9, 7.8, 7.8, 6.3, 6.1, 4.0)/100
#' shares2seats(85, res2021)
#' }
shares2seats <- function(seats, shares, threshold=0.03) {
  if (!isTRUE(all.equal(sum(shares), 1))) {
    warning("Vote shares do not add up to 1", call.=FALSE)
  } 
  shares_above_thr <- shares >= threshold
  parties_above_thr <- which(shares_above_thr)
  valid_shares_above_thr <- shares[shares_above_thr]
  allocation <- rep(0, length(shares))
  allocation[parties_above_thr] <- dhondt(seats, valid_shares_above_thr)
  return(allocation)
}

#' Simulate vote shares for different parties
#'
#' Simulates vote shares extracted from a normal distribution centered
#' at \code{shares} and with standard deviation \code{sigma}
#' 
#' @param shares A vector of vote shares for each party
#' @param sigma A vector of vote share uncertainty for ech party
#' @param N An integer with the number of simulations
#' @return A matrix of dimensions \code{length(p)} x N with the
#'   simulated vote shares
simulate_vote_share <- function(shares, sigmas, N, ...) {  
  l <- length(shares) 
  simshare <- mapply(function(x, y) rnorm(N, x, y),
                     shares,
                     sigmas)
  if (any(simshare < 0) | any(simshare > 1)) {
    warning("Some simulate vote shares are below 0 or above 1.", call.=FALSE)
  }
  return(simshare)
}


#' Simulate seat allocations
#'
#' @param shares A vector of vote shares for each party.
#' @param sigma A vector of vote share uncertainty for ech party.
#' @param N An integer with the number of simulations. 
#' @param threshold The electoral threshold (between 0 and 1)
#' @return A matrix with the simulated seat distribution
.simulate <- function(seats, shares, sigmas, N, threshold, ...) {
  if (length(shares) != length(sigmas)) {
    stop("Length of shares and sigmas is not equal", call.=FALSE)
  }
  simulated_vote_shares <- simulate_vote_share(shares, sigmas, N)
  res <- apply(simulated_vote_shares,
               1,
               function(x) shares2seats(seats, x, threshold),
               simplify=FALSE)
  sims <- do.call(rbind, res)
  return(sims)
}

#' Simulate seat allocations
#'
#' Simulates seat allocations for Catalunya using a normal
#' distribution for uncertainty and applying D'Hondt's formula at the
#' circunscription level.
#'
#' @param shares A \code{data.frame} columns named after each
#'   circunscription ("Barcelona", "Girona", "Tarragona", "Lleida")
#'   that contains the vote shares for each party.
#' @param sigma A \code{data.frame} with the same structure as
#'   \code{shares} but with the vote share uncertainty for each party.
#' @param names An optional vector of party names. Defaults to NULL.
#'   The length must be equal to the number of rows in \code{shares}.
#' @param N An integer with the number of simulations. Default is
#'   1000.
#' @param threshold The electoral threshold (between 0 and 1) for each
#'   circunscription. Default is 0.03.
#' @return An array with dimensions (\code{N}, number of parties, 4)
#'   with the simulated seat allocation for each party in each
#'   circunscription.
#' @examples
#' \dontrun{
#' # Vote shares
#' res2021 <- data.frame("Barcelona"=c(25.0, 20.4, 17.9, 7.8, 7.8, 6.3, 6.1, 4.0),
#'                       "Girona"=c(15.2, 21.8, 32.7, 6.1, 4.0, 9.0, 3.3, 2.0),
#'                       "Lleida"=c(15.0, 26.6, 28.0, 5.5, 3.2, 7.4, 3.2, 3.5),
#'                       "Tarragona"=c(20.0, 24.5, 19.4, 9.4, 4.9, 6.8, 5.2, 4.3))/100
#' # Uncertainty
#' sig2021 <- data.frame("Barcelona"=rep(.01, 8),
#'                       "Girona"=rep(.01, 8),
#'                       "Lleida"=rep(.01, 8),
#'                       "Tarragona"=rep(.01, 8))
#' # Party names
#' parties <- c("PSC", "ERC", "JxCat", "Vox", "ECP–PEC", "CUP–G", "Cs", "PP")
#' # Simulation
#' res <- simulate(res2021, sig2021, parties)
#' }
#' @export
simulate <- function(shares,
                     sigmas,
                     names=NULL,
                     N=1000,
                     threshold=0.03,
                     allocations=c("Barcelona"=85, "Girona"=17, "Tarragona"=15, "Lleida"=18)) {
  if (!is.data.frame(shares) | !is.data.frame(sigmas)) {
    stop(sprintf("%s and %s must be data.frames", sQuote("shares"), sQuote("sigmas")), call.=FALSE)
  }
  if (!isTRUE(all.equal(dim(shares), dim(sigmas)))) {
    stop(sprintf("%s and %s must have same dimensions", sQuote("shares"), sQuote("sigmas")), call.=FALSE)
  }
  
  if (!setequal(names(shares), names(allocations)) |
        !setequal(names(shares), names(sigmas))) {
    stop(sprintf("The names of %s, %s and %s do not match",
                 sQuote("shares"),
                 sQuote("sigmas"),
                 sQuote("allocations")
                 ))
  }
  
  if (!is.null(names)) {
    if (length(names) != nrow(shares)) {
      stop(sprintf("The length of %s must be equal to the number of rows of %s",
                   sQuote("names"),
                   sQuote("shares")),
           call.=FALSE)
    }
    else row.names(shares) <- names
  }
  
  res <- array(NA,
               dim=c(N, nrow(shares), length(allocations)),
               dimnames=list(1:N,
                             row.names(shares),
                             names(shares)))
  for (i in seq_along(allocations)) {
    simulation <- try(.simulate(seats=allocations[i],
                                shares=shares[, names(allocations[i])],
                                sigmas=sigmas[, names(allocations[i])],
                                N=N,
                                threshold=threshold),
                      silent=TRUE)
    if (inherits(simulation, "try-error")) {
      msg <- sprintf("Could not simulate %s: %s",
                     names(allocations[i]),
                     attr(simulation, "condition")$message)
      stop(msg, call.=FALSE)
    }
    res[, , i] <- simulation
  }
  class(res) <- "simulation"
  return(res)
}


#' Coerce a simulation to a data frame
#'
#' @param x An object of class \code{simulation}.
#' @return A data frame with columns indicating the seat allocation
#'   for each party at each circunscription and each simulation.
#' 
#' @export
as.data.frame.simulation <- function(x) {
  dims <- dimnames(x)
  x <- array(aperm(x, c(2, 1, 3)), c(dim(x)[1] * dim(x)[2], dim(x)[3]))
  attr(x, "class") <- NULL
  x <- as.data.frame(x)
  names(x) <- dims[[3]]
  x$total <- rowSums(x)
  x$simulation <- rep(dims[[1]], each=length(dims[[2]]))
  x$party <- rep(dims[[2]], length(dims[[1]]))
  return(x)
}

