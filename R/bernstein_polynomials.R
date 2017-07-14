#' @include utils.R
NULL

#' Multidimensional lattice for Bernstein approximation.
#'
#' Generates a lattice in \code{d} dimensions in each ranging from
#' \code{0} through \code{k}, in some fixed order. For internal use only.
#'
#' @param d positive scalar integer dimension.
#' @param k positive scalar integer max lattice point component.
#' @return a matrix with \eqn{(k+1)^d} rows and \eqn{d} columns enumerating
#'   the lattice points in rows.
.bernstein_lattice <- function(d, k) {
  if (!.check_integer(d) || d < 1) {
    stop("Expected positive scalar integer d.")
  }
  if (!.check_integer(k) || k < 1) {
    stop("Expected positive scalar integer k.")
  }
  return(as.matrix(expand.grid(replicate(d, 0:k, simplify = FALSE))))
}

#' Fit a Bernstein polynomial approximation.
#'
#' Fits the basis of Bernstein polynomial functions to given real-valued
#' function \code{f} of \eqn{\[0,1\]^d} where \eqn{d=}\code{dims}, against
#' a regular lattice of \eqn{k+1} points in each dimension for given
#' \code{k}. Note the approximation is not the iterated variant.
#'
#' @param f a function to be approximated.
#' @param dims the function \code{f}'s domain dimension.
#' @param k the lattice resolution of approximation.
#' @return an S3 object of class \code{bernstein}.
#'
#' @seealso \code{\link{predict.bernstein}} for subsequent evaluation.
#'
#' @examples
#' f <- function(x) x * sin(x*10)
#' b <- bernstein(f, dims = 1)
#' xs <- seq(from=0, to=1, length=50)
#' mean((f(xs) - predict(b,xs))^2)
#'
#' @references
#' Francesco Aldà and Benjamin I. P. Rubinstein. "The Bernstein Mechanism:
#'   Function Release under Differential Privacy", in Proceedings of the 31st
#'   AAAI Conference on Artificial Intelligence (AAAI'2017), pp. 1705-1711,
#'   Feb 2017.
#'
#' @export
bernstein <- function(f, dims, k = 10) {
  if (!is.function(f)) {
    stop("Expected f a function.")
  }
  if (!.check_integer(dims) || dims < 1) {
    stop("Expected positive scalar integer dims.")
  }
  if (!.check_integer(k) || k < 1) {
    stop("Expected positive scalar integer k.")
  }
  latt <- .bernstein_lattice(d = dims, k = k) / k
  r <- structure(list(dims = dims, k = k, coeffs = apply(latt, 1, f)),
                 class = "bernstein")
  return(r)
}

#' Evaluate Bernstein approximations on data.
#'
#' Evaluates a given S3 object of type \code{bernstein} on given
#' data \code{D}.
#'
#' @param object an S3 object of type \code{bernstein}.
#' @param D either a numeric vector or matrix, all values in \code{[0,1]}.
#'   If numeric then length should be \code{object$dims} unless the latter is
#'   1 in which case the length can be arbitrary. If a matrix then the number
#'   of columns should match \code{object$dims}.
#' @param \dots additional arguments.
#' @return a numeric vector of scalar real evaluations.
#'
#' @examples
#' f <- function(x) x * sin(x*10)
#' b <- bernstein(f, dims = 1)
#' xs <- seq(from=0, to=1, length=50)
#' mean((f(xs) - predict(b,xs))^2)
#'
#' @references
#' Francesco Aldà and Benjamin I. P. Rubinstein. "The Bernstein Mechanism:
#'   Function Release under Differential Privacy", in Proceedings of the 31st
#'   AAAI Conference on Artificial Intelligence (AAAI'2017), pp. 1705-1711,
#'   Feb 2017.
#'
#' @export
predict.bernstein <- function(object, D, ...) {
  if (class(object) != "bernstein") {
    stop("Expected object of type 'bernstein'.")
  }
  if (!is.numeric(D) || any(D < 0) || any(D > 1)) {
    stop("Expected numeric data in unit interval [0,1].")
  }
  if (is.matrix(D) && ncol(D) != object$dims) {
    stop("Expected object$dims columns in matrix D.")
  }
  if (!is.matrix(D)) {
    if (object$dims <= 1) {
      D <- as.matrix(D, ncol = 1)
    } else {
      if (length(D) != object$dims) {
        stop("Expected numeric D of length object$dims.")
      }
      D <- matrix(D, nrow = 1, ncol = object$dims)
    }
  }
  latt <- .bernstein_lattice(d = object$dims, k = object$k)
  oneEval <- function(ys) {
    pmfs <- apply(latt, 1,
                  stats::dbinom,
                  size = rep(object$k, object$dims),
                  prob = ys)
    if (object$dims > 1) {
      pmfs <- apply(pmfs, 2, prod)
    }
    return(sum(pmfs * object$coeffs))
  }
  return(apply(D, 1, oneEval))
}
