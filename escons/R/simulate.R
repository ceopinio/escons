#' Simulate vote shares for different parties
#'
#' Simulates vote shares extracted from a normal distribution centered
#' at \code{shares} and with standard deviation \code{sigma}
#' 
#' @param shares A vector of vote shares for each party
#' @param sigmas A vector of vote share uncertainty for ech party
#' @param N An integer with the number of simulations
#' @param ... Additional arguments (currently not used)
#' @return A matrix of dimensions \code{length(p)} x N with the
#'   simulated vote shares
#'
#' @importFrom stats rnorm
simulate_vote_share <- function(shares, sigmas, N, ...) {  
  simshare <- mapply(function(x, y) rnorm(N, x, y),
                     shares,
                     sigmas)
  if (any(simshare < 0) | any(simshare > 1)) {
    warning("Some simulated vote shares are below 0 or above 1.", call.=FALSE)
  }
  return(simshare)
}


#' Simulate seat allocations
#'
#' @param seats An integer with the total number of seats
#' @param shares A vector of vote shares for each party.
#' @param sigmas A vector of vote share uncertainty for ech party.
#' @param N An integer with the number of simulations. 
#' @param threshold The electoral threshold (between 0 and 1)
#' @param ... Additional arguments (currently not used)
#' @return A matrix with the simulated seat distribution
.simulate <- function(seats, shares, sigmas, N, threshold, ...) {
  if (length(shares) != length(sigmas)) {
    stop(sprintf("Length of %s and %s is not equal",
                 sQuote("shares"),
                 sQuote("sigmas")), call.=FALSE)
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
#' district level.
#'
#' @param shares A \code{data.frame} columns named after each district
#' @param sigmas A \code{data.frame} with the same structure as
#'   \code{shares} but with the vote share uncertainty for each party.
#' @param names An optional vector of party names. Defaults to NULL.
#'   The length must be equal to the number of rows in \code{shares}.
#' @param N An integer with the number of simulations. Default is
#'   1000.
#' @param threshold The electoral threshold (between 0 and 1) for each
#'   district. Default is 0.03.
#' @param dsize A named vector with the district size
#' @return An array with dimensions (\code{N}, number of parties,
#'   number of districts) with the simulated seat allocation for each
#'   party in each district.
#'
#' @examples
#' \dontrun{
#' # Vote shares
#' res2021 <- data.frame("Barcelona"=c(25.0, 20.4, 17.9, 7.8, 7.8, 6.3, 6.1, 4.0),
#'                       "Girona"=c(15.2, 21.8, 32.7, 6.1, 4.0, 9.0, 3.3, 2.0),
#'                       "Lleida"=c(15.0, 26.6, 28.0, 5.5, 3.2, 7.4, 3.2, 3.5),
#'                       "Tarragona"=c(20.0, 24.5, 19.4, 9.4, 4.9, 6.8, 5.2, 4.3))/100
#' # Uncertainty
#' sig2021 <- moe(res2021, N=3000, level=.95) # 3000 in *each* district
#' # Party names
#' parties <- c("PSC", "ERC", "JxCat", "Vox", "ECP–PEC", "CUP–G", "Cs", "PP")
#' # Simulation
#' res <- simulate(res2021, sig2021, parties)
#' # Transform to data.frame for analysis
#' res <- as.data.frame(res)
#' }
#' @export
simulate <- function(shares,
                     sigmas=NULL,
                     names=NULL,
                     N=1000,
                     threshold=0.03,
                     dsize=c("Barcelona"=85,
                             "Girona"=17,
                             "Tarragona"=18,
                             "Lleida"=15)) {
  if (!is.data.frame(shares) | !is.data.frame(sigmas)) {
    stop(sprintf("%s and %s must be data.frames",
                 sQuote("shares"),
                 sQuote("sigmas")), call.=FALSE)
  }
  if (!isTRUE(all.equal(dim(shares), dim(sigmas)))) {
    stop(sprintf("%s and %s must have same dimensions",
                 sQuote("shares"),
                 sQuote("sigmas")), call.=FALSE)
  }
  if (!is.numeric(dsize)) {
    stop(sprintf("%s must be a vector", sQuote("dsize")), call.=FALSE)
  }
  if (!setequal(names(shares), names(dsize)) |
        !setequal(names(shares), names(sigmas))) {
    stop(sprintf("The names of %s, %s and %s do not match",
                 sQuote("shares"),
                 sQuote("sigmas"),
                 sQuote("dsize")
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
               dim=c(N, nrow(shares), length(dsize)),
               dimnames=list(1:N,
                             row.names(shares),
                             names(dsize)))
  for (i in seq_along(dsize)) {
    simulation <- try(.simulate(seats=dsize[i],
                                shares=shares[, names(dsize[i])],
                                sigmas=sigmas[, names(dsize[i])],
                                N=N,
                                threshold=threshold),
                      silent=TRUE)
    if (inherits(simulation, "try-error")) {
      msg <- sprintf("Could not simulate %s: %s",
                     names(dsize[i]),
                     attr(simulation, "condition")$message)
      stop(msg, call.=FALSE)
    }
    res[, , i] <- simulation
  }
  class(res) <- "simulation"
  return(res)
}


#' @method as.data.frame simulation
#' @export
as.data.frame.simulation <- function(x, row.names, optional, enrich=TRUE, ...) {
  dims <- dimnames(x)
  x <- array(aperm(x, c(2, 1, 3)), c(dim(x)[1] * dim(x)[2], dim(x)[3]))
  attr(x, "class") <- NULL
  x <- as.data.frame(x)
  names(x) <- dims[[3]]
  if (enrich) {
    x$total <- rowSums(x)
    x$party <- rep(dims[[2]], length(dims[[1]]))    
  }
  x$simulation <- rep(dims[[1]], each=length(dims[[2]]))  
  return(x)
}
