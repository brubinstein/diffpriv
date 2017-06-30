#' @include mechanisms.R privacy_params.R utils.R
NULL

#' An S4 class for the Laplace mechanism of differential privacy.
#'
#' A class that implements the basic Laplace mechanism of differential privacy,
#' for privatizing numeric vector releases.
#'
#' @slot sensitivity non-negative scalar numeric target sensitivity.
#' @slot target the target non-private function to be privatized.
#' @slot gammaSensitivity \code{NA_real_} if deactive, or scalar in [0,1)
#'   indicating that responses must be RDP with specific confidence.
#' @slot dim positive scalar numeric dimension of responses.
#'
#' @references Cynthia Dwork, Frank McSherry, Kobbi Nissim, and Adam Smith.
#'   "Calibrating noise to sensitivity in private data analysis." In Theory of
#'   Cryptography Conference, pp. 265-284. Springer Berlin Heidelberg, 2006.
#'
#' @export DPMechLaplace
#' @exportClass DPMechLaplace
DPMechLaplace <- setClass("DPMechLaplace",
  contains = "DPMech",
  slots = list(dim = "numeric")
)

## A \code{DPMechLaplace} should be constructed with an appropriate dimension.
setValidity("DPMechLaplace", function(object) {
  if (!.check_integer(object@dim)) {
    return("DPMechLaplace@dim should be a scalar natural number.")
  }
  return(TRUE)
})

#' @describeIn DPMechLaplace automatically prints the object.
#' @param object an instance of class \code{DPMech}.
setMethod("show", "DPMechLaplace", function(object) {
  cat("Laplace mechanism\n")
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

#' @describeIn DPMechLaplace releases Laplace mechanism responses.
#' @param mechanism an object of class \code{\link{DPMechLaplace}}.
#' @param privacyParams an object of class \code{\link{DPParamsEps}}.
#' @param X a privacy-sensitive dataset, a list if using sensitivity sampler.
#' @return list with slots per argument, actual privacy parameter; Laplace
#'   mechanism response with length of target release:
#'   \code{privacyParams, sensitivity, dim, target, response}.
#' @examples
#' f <- function(xs) mean(xs)
#' n <- 100
#' m <- DPMechLaplace(sensitivity = 1/n, target = f, dim = 1)
#' X <- runif(n)
#' p <- DPParamsEps(epsilon = 1)
#' releaseResponse(m, p, X)
#' @export
setMethod("releaseResponse",
  signature(mechanism = "DPMechLaplace",
            privacyParams = "DPParamsEps",
            X = "ANY"),
  function(mechanism, privacyParams, X) {
    rawR <- mechanism@target(X)
    if (!is.numeric(rawR)) {
      stop("Non-private target output non-numeric.")
    }
    if (length(rawR) != mechanism@dim) {
      warning("Non-private target output has unexpected dimension.")
    }
    noise <- .rlap(length(rawR),
                   location = 0,
                   scale = mechanism@sensitivity / privacyParams@epsilon)
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

#' @describeIn DPMechLaplace measures sensitivity of non-private \code{target}.
#' @param X1 a privacy-sensitive dataset, list if sensitivity sampler compatible.
#' @param X2 a privacy-sensitive dataset, list if sensitivity sampler compatible.
#' @return scalar numeric norm of non-private \code{target} on datasets.
#' @examples
#' f <- function(xs) mean(xs)
#' n <- 100
#' m <- DPMechLaplace(sensitivity = 1/n, target = f, dim = 1)
#' X1 <- runif(n)
#' X2 <- runif(n)
#' sensitivityNorm(m, X1, X2)
#' @export
setMethod("sensitivityNorm",
  signature(mechanism = "DPMechLaplace",
            X1 = "ANY",
            X2 = "ANY"),
  function(mechanism, X1, X2) {
    rawR1 <- mechanism@target(X1)
    rawR2 <- mechanism@target(X2)
    if (!is.numeric(rawR1) || !is.numeric(rawR2)) {
      stop("Non-private target output non-numeric.")
    }
    if (length(rawR1) != mechanism@dim || length(rawR2) != mechanism@dim) {
      warning("Non-private target output has unexpected dimension.")
    }
    if (length(rawR1) != length(rawR2)) {
      stop("Non-private target output dimensions inconsistent.")
    }
    if (length(rawR1) == 0) {
      return(0)
    }
    return(.l1norm(rawR1 - rawR2))
  }
)
