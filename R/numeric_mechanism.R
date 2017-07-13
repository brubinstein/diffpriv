#' @include mechanisms.R privacy_params.R utils.R
NULL

#' A virtual S4 class for differentially-private numeric mechanisms.
#'
#' A virtual class that implements common features of Laplace, Gaussian
#' mechanisms from differential privacy, for privatizing numeric vector
#' releases.
#'
#' @slot sensitivity non-negative scalar numeric target sensitivity.
#'   Defaults to \code{Inf} for use with \code{sensitivitySampler()}.
#' @slot target the target non-private function to be privatized, takes lists.
#'   Defaults to a constant function. Laplace mechanism assumes functions that
#'   release numeric vectors of fixed dimension \code{dims}.
#' @slot gammaSensitivity \code{NA_real_} if deactive, or scalar in [0,1)
#'   indicating that responses must be RDP with specific confidence.
#' @slot dims positive scalar numeric dimension of responses. Defaults to
#'   \code{NA_integer_} for use with \code{sensitivitySampler()} which can
#'   probe \code{target} to determine dimension.
#'
#' @export DPMechNumeric
#' @exportClass DPMechNumeric
DPMechNumeric <- setClass("DPMechNumeric",
  contains = c("DPMech", "VIRTUAL"),
  slots = list(dims = "numeric"),
  prototype = prototype(dims = NA_integer_)
)

## A \code{DPMechNumeric} should be constructed with an appropriate dimension.
setValidity("DPMechNumeric", function(object) {
  if (!is.na(object@dims) && !.check_integer(object@dims)) {
    return("DPMechNumeric@dims should be a scalar natural number.")
  }
  return(TRUE)
})

#' @describeIn DPMechNumeric automatically prints the object.
#' @param object an instance of class \code{DPMech}.
setMethod("show", "DPMechNumeric", function(object) {
  cat("Sensitivity:", object@sensitivity, "\n")
  if (is.na(object@gammaSensitivity)) {
    cat("Sampled sensitivity gamma: NA\n")
  } else {
    cat("Sampled sensitivity gamma:", object@gammaSensitivity, "\n")
  }
  cat("Response dimension:", object@dims, "\n")
  cat("Target function: \n")
  show(object@target)
})

#' @describeIn DPMechNumeric measures sensitivity of non-private \code{target}.
#' @param mechanism an object of class \code{DPMechNumeric-class}.
#' @param X1 a privacy-sensitive dataset.
#' @param X2 a privacy-sensitive dataset.
#' @return scalar numeric norm of non-private \code{target} on datasets.
#' @examples
#' f <- function(xs) mean(xs)
#' n <- 100
#' m <- DPMechLaplace(sensitivity = 1/n, target = f, dims = 1)
#' X1 <- runif(n)
#' X2 <- runif(n)
#' sensitivityNorm(m, X1, X2)
#' @export
setMethod("sensitivityNorm",
  signature(mechanism = "DPMechNumeric",
            X1 = "ANY",
            X2 = "ANY"),
  function(mechanism, X1, X2) {
    rawR1 <- mechanism@target(X1)
    rawR2 <- mechanism@target(X2)
    if (!is.numeric(rawR1) || !is.numeric(rawR2)) {
      stop("Non-private target output non-numeric.")
    }
    if (is.na(mechanism@dims)) {
      warning("No expected dimension set.")
    } else {
      if (length(rawR1) != mechanism@dims || length(rawR2) != mechanism@dims) {
        warning("Non-private target output has unexpected dimension.")
      }
    }
    if (length(rawR1) != length(rawR2)) {
      stop("Non-private target output dimensions inconsistent.")
    }
    if (length(rawR1) == 0) {
      return(0)
    }
    return(.numericNorm(mechanism, rawR1, rawR2))
  }
)

#' \code{DPMechNumeric} response space norm.
#'
#' Represents the norm of \code{target} responses. For internal use.
#'
#' @param object an instance of class \code{\link{DPMechNumeric-class}}.
#' @param rawR1 a non-private response from \code{target}.
#' @param rawR2 a non-private response from \code{target}.
#' @return a non-negative scalar norm between \code{rawR1}, \code{rawR2}.
setGeneric(".numericNorm", function(object, rawR1, rawR2) {
  standardGeneric(".numericNorm")
})

#' \code{DPMechNumeric} release response core.
#'
#' Implements the core calculation specific to the \code{DPMechNUmeric}
#' subclass \code{releaseResponse()}. Internal function, not to be called.
#'
#' @param object an instance of class \code{\link{DPMechNumeric-class}}.
#' @param rawR a non-private response from \code{target}.
#' @param privacyParams object of class (or subclass of) \code{DPParamsEps}.
#' @return a numeric private response.
setGeneric(".responseCore", function(object, rawR, privacyParams) {
  standardGeneric(".responseCore")
})

#' @describeIn DPMechNumeric releases mechanism responses.
#' @param privacyParams an object of class \code{\link{DPParamsEps}}.
#' @param X a privacy-sensitive dataset, if using sensitivity sampler a: list,
#'   matrix, data frame, numeric/character vector.
#' @return list with slots per argument, actual privacy parameter;
#'   mechanism response with length of target release:
#'   \code{privacyParams, sensitivity, dims, target, response}.
#' @examples
#' f <- function(xs) mean(xs)
#' n <- 100
#' m <- DPMechLaplace(sensitivity = 1/n, target = f, dims = 1)
#' X <- runif(n)
#' p <- DPParamsEps(epsilon = 1)
#' releaseResponse(m, p, X)
#' @export
setMethod("releaseResponse",
  signature(mechanism = "DPMechNumeric",
            privacyParams = "DPParamsEps",
            X = "ANY"),
  function(mechanism, privacyParams, X) {
    rawR <- mechanism@target(X)
    if (!is.numeric(rawR)) {
      stop("Non-private target output non-numeric.")
    }
    if (is.na(mechanism@dims)) {
      warning("No expected non-private dims slot set.")
    }
    if (length(rawR) != mechanism@dims) {
      warning("Non-private target output has unexpected dimension.")
    }
    R <- .responseCore(mechanism, rawR = rawR, privacyParams = privacyParams)
    if (is.na(mechanism@gammaSensitivity)) {
      p <- privacyParams
    } else {
      p <- toGamma(privacyParams, mechanism@gammaSensitivity)
    }
    return(list(
      privacyParams = p,
      sensitivity = mechanism@sensitivity,
      dims = mechanism@dims,
      target = mechanism@target,
      response = R
    ))
  }
)
