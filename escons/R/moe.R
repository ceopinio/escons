#' Calculate margin of error of a proportion
#'
#' Calculates margin of error of a proportion for a given confidence
#' level and sample size
#'
#' @param x A numeric vector or data.frame
#' @param N The sample size. If \code{x} is a data.frame, N can be a
#'   vector of length equal to the number of columns of \code{x}
#' @param level The confidence level (between 0 and 1)
#' @param ... Additional parameters (currently not used)
#' @examples
#' \dontrun{
#' moe(.5, N=1000, level=.95)
#' moe(res2021, N=3000, level=.95)
#' }
#' @export
moe <- function(x, ...) {
  UseMethod("moe")
}


#' @rdname moe
#' @importFrom stats qnorm
#' @export
moe.default <- function(x, N, level, ...) {
  if (any(x < 0) | any(x > 1)) {
    stop("Input value must be a proportion between 0 and 1")
  }
  if (level < 0 | level > 1) {
    stop("The confidence level must be a value between 0 and 1")
  }
  z <- qnorm(1 - (1 - level)/2)
  res <- z * sqrt((x * (1 - x)) / N)
  if (any(res < 0)) {
    stop("MoE is less than zero. Is the confidence level correct?")
  }
  return(res)
}


#' @rdname moe
#' @export
moe.numeric <- function(x, N, level, ...) {
  return(moe.default(x, N, level))
}


#' @rdname moe
#' @export
moe.data.frame <- function(x, N, level, ...) {
  res <- mapply(\(data, size) moe.default(data, size, level), data=x, size=N, SIMPLIFY=FALSE)
  return(as.data.frame(do.call(cbind, res)))
}
