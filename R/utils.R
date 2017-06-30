#' Check is Integral.
#'
#' Checks that a numeric has no fractional part.
#'
#' While \link[base]{is.integer} returns \code{TRUE} when given an object of type
#' \code{integer}, it always returns \code{FALSE} on non-floating $\code{numeric}s.
#' This function ignores class, and indicates absence of fractional part.
#'
#' @param n a singleton numeric.
#' @return Boolean that \code{n} is singleton numeric with no fractional part.
#'
#' @seealso \link[base]{is.integer} for checking for objects of type \code{integer}.
#'
#' @examples
#' .check_integer(5)             # TRUE
#' .check_integer(5.0)           # FALSE
#' .check_integer(as.integer(5)) # TRUE
.check_integer <- function(n) {
  is.numeric(n) && length(n) == 1 && n %% 1 == 0
}

#' The Laplace Distribution
#'
#' Random generation for the Laplace distribution with location \code{location}
#' and scale \code{scale}.
#'
#' If \code{location} is not specified, it assumes the default value of \code{0}.
#' If \code{scale} is not specified, it assumes the default value of \code{1}.
#'
#' The Laplace distribution with real location \eqn{\mu} and scale \eqn{b>0} has
#' density
#' \deqn{f(x)=\frac{1}{2b}\exp\left(-\frac{|x-\mu|}{b}\right)}{%
#'       f(x)=exp(-|x-\mu|/b)/(2b)}
#' for real \eqn{x}.
#'
#' @param n of observations (singleton numeric).
#' @param location the location (singleton numeric).
#' @param scale the scale (singelton numeric).
#' @return The length of the numeric result is determined by \code{n}.
#'
#' @source Uses \link[stats]{rexp}.
#'
#' @seealso \link[stats]{rexp} for random generation from the exponential distribution.
#'
#' @examples
#' rlap(5)
#' rlap(5, location=5)
#' rlap(5, location=5, scale=0.5)
.rlap <- function(n, location = 0, scale = 1) {
  if (!.check_integer(n) || n < 0)
    stop("Given sample size is not scalar non-negative integer.")
  if (!is.numeric(location) | length(location) != 1)
    stop("Given location is a not a scalar numeric.")
  if (!is.numeric(scale) || length(scale) != 1 || scale < 0)
    stop("Given scale is not scalar non-negative.")
  if (scale <= 0)
    rep(location, n)
  rep(location, n)
  + stats::rexp(n = n, rate = 1/scale)
  - stats::rexp(n = n, rate = 1/scale)
}

#' The L1 norm.
#'
#' L1 norm of a numeric vector.
#'
#' Returns \code{0} for \code{xs} of length zero; otherwise the sum of absolutes.
#'
#' @param xs a numeric vector.
#' @return The sum of absolutes of \code{xs}.
#'
#' @examples
#' xs <- c(-2.5, 1, 2)
#' .l1norm(xs)    # 5.5
.l1norm <- function(xs) {
  if (!is.numeric(xs))
    stop("Given object is not numeric.")
  if (length(xs) == 0)
    return(0)
  return(sum(abs(xs)))
}

#' The L_Infty norm.
#'
#' L_Infty norm of a numeric vector.
#'
#' Returns \code{0} for \code{xs} of length zero; otherwise the max of absolutes.
#'
#' @param xs a numeric vector.
#' @return The max of absolutes of \code{xs}.
#'
#' @examples
#' xs <- c(-2.5, 1, 2)
#' .linfty_norm(xs)    # 2.5
.linfty_norm <- function(xs) {
  if (!is.numeric(xs))
    stop("Given object is not numeric.")
  if (length(xs) == 0)
    return(0)
  return(max(abs(xs)))
}

#' Constant-valued function.
#'
#' A default target function that outputs response zero.
#'
#' @param X arbitrary dataset object.
#' @return A response that does not depend on \code{X}.
.constant_target <- function(X) {
  return(0)
}
