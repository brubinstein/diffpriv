#' @include mechanisms.R privacy_params.R utils.R
NULL

#' An S4 class for the Gaussian mechanism of differential privacy.
#'
#' A class that implements the Gaussian mechanism of differential privacy,
#' for privatizing numeric vector releases.
#'
#' @slot sensitivity non-negative scalar numeric L2 target sensitivity.
#'   Defaults to \code{Inf} for use with \code{sensitivitySampler()}.
#' @slot target the target non-private function to be privatized, takes lists.
#'   Defaults to a constant function. Gaussian mechanism assumes functions that
#'   release numeric vectors of fixed dimension \code{dim}.
#' @slot gammaSensitivity \code{NA_real_} if deactive, or scalar in [0,1)
#'   indicating that responses must be RDP with specific confidence.
#' @slot dim positive scalar numeric dimension of responses. Defaults to
#'   \code{NA_integer_} for use with \code{sensitivitySampler()} which can
#'   probe \code{target} to determine dimension.
#'
#' @references Cynthia Dwork and Aaron Roth. "Algorithmic Foundations of
#' Differential Privacy" Foundations and Trends in Theoretical Computer
#' Science. Now Publishers, 2014.
#'
#' @export DPMechGaussian
#' @exportClass DPMechGaussian
DPMechGaussian <- setClass("DPMechGaussian",
  contains = "DPMech",
  slots = list(dim = "numeric"),
  prototype = prototype(dim = NA_integer_)
)

## A \code{DPMechGaussian} should be constructed with an appropriate dimension.
setValidity("DPMechGaussian", function(object) {
  if (!is.na(object@dim) && !.check_integer(object@dim)) {
    return("DPMechGaussian@dim should be a scalar natural number.")
  }
  return(TRUE)
})

#' @describeIn DPMechGaussian automatically prints the object.
#' @param object an instance of class \code{DPMech}.
setMethod("show", "DPMechGaussian", function(object) {
  cat("Gaussian mechanism\n")
  cat("Sensitivity:", object@sensitivity, "\n")
  if (is.na(object@gammaSensitivity)) {
    cat("Sampled sensitivity gamma: NA\n")
  } else {
    cat("Sampled sensitivity gamma:", object@gammaSensitivity, "\n")
  }
  cat("Response dimension:", object@dim, "\n")
  cat("Target function: \n")
  show(object@target)
})

#' @describeIn DPMechGaussian releases Gaussian mechanism responses.
#' @param mechanism an object of class \code{\link{DPMechGaussian}}.
#' @param privacyParams an object of class \code{\link{DPParamsDel}}.
#' @param X a privacy-sensitive dataset, if using sensitivity sampler a: list,
#'   matrix, data frame, numeric/character vector.
#' @return list with slots per argument, actual privacy parameter; Gaussian
#'   mechanism response with length of target release:
#'   \code{privacyParams, sensitivity, dim, target, response}.
#' @examples
#' f <- function(xs) mean(xs)
#' n <- 100
#' m <- DPMechGaussian(sensitivity = 1/n, target = f, dim = 1)
#' X <- runif(n)
#' p <- DPParamsDel(epsilon = 1, delta = 0.1)
#' releaseResponse(m, p, X)
#' @export
setMethod("releaseResponse",
  signature(mechanism = "DPMechGaussian",
            privacyParams = "DPParamsDel",
            X = "ANY"),
  function(mechanism, privacyParams, X) {
    rawR <- mechanism@target(X)
    if (!is.numeric(rawR)) {
      stop("Non-private target output non-numeric.")
    }
    if (is.na(mechanism@dim)) {
      warning("No expected non-private dim slot set.")
    }
    if (length(rawR) != mechanism@dim) {
      warning("Non-private target output has unexpected dimension.")
    }
    C <- sqrt(2 * log(1.25 / privacyParams@delta))
    noise <- stats::rnorm(length(rawR), mean = 0,
      sd = mechanism@sensitivity * C / privacyParams@epsilon)
    R <- rawR + noise
    if (is.na(mechanism@gammaSensitivity)) {
      p <- privacyParams
    } else {
      p <- toGamma(privacyParams, mechanism@gammaSensitivity)
    }
    return(list(
      privacyParams = p,
      sensitivity = mechanism@sensitivity,
      dim = mechanism@dim,
      target = mechanism@target,
      response = R
    ))
  }
)

#' @describeIn DPMechGaussian measures sensitivity of non-private \code{target}.
#' @param X1 a privacy-sensitive dataset, list if sensitivity sampler compatible.
#' @param X2 a privacy-sensitive dataset, list if sensitivity sampler compatible.
#' @return scalar numeric norm of non-private \code{target} on datasets.
#' @examples
#' f <- function(xs) mean(xs)
#' n <- 100
#' m <- DPMechGaussian(sensitivity = 1/n, target = f, dim = 1)
#' X1 <- runif(n)
#' X2 <- runif(n)
#' sensitivityNorm(m, X1, X2)
#' @export
setMethod("sensitivityNorm",
  signature(mechanism = "DPMechGaussian",
            X1 = "ANY",
            X2 = "ANY"),
  function(mechanism, X1, X2) {
    rawR1 <- mechanism@target(X1)
    rawR2 <- mechanism@target(X2)
    if (!is.numeric(rawR1) || !is.numeric(rawR2)) {
      stop("Non-private target output non-numeric.")
    }
    if (is.na(mechanism@dim)) {
      warning("No expected dimension set.")
    } else {
      if (length(rawR1) != mechanism@dim || length(rawR2) != mechanism@dim) {
        warning("Non-private target output has unexpected dimension.")
      }
    }
    if (length(rawR1) != length(rawR2)) {
      stop("Non-private target output dimensions inconsistent.")
    }
    if (length(rawR1) == 0) {
      return(0)
    }
    return(.l2norm(rawR1 - rawR2))
  }
)
