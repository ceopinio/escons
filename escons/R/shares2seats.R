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
    warning(sprintf("%s ties were found during the seat allocation", ties),
            call.=FALSE)
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
  if (is.null(sys.call(-1))) { ## If called directly
    if (any(shares < 0) | any(shares > 1)) {
      stop("Vote shares must be between 0 and 1", call.=FALSE)
    }
  }
    
  shares_above_thr <- shares >= threshold
  parties_above_thr <- which(shares_above_thr) ## Make room for those below thr
  valid_shares_above_thr <- shares[shares_above_thr]
  allocation <- rep(0, length(shares))
  allocation[parties_above_thr] <- dhondt(seats, valid_shares_above_thr)
  return(allocation)
}
