#' @include mechanisms.R numeric_mechanism.R privacy_params.R utils.R
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
#'   release numeric vectors of fixed dimension \code{dims}.
#' @slot gammaSensitivity \code{NA_real_} if deactive, or scalar in [0,1)
#'   indicating that responses must be RDP with specific confidence.
#' @slot dims positive scalar numeric dimension of responses. Defaults to
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
  contains = "DPMechNumeric"
)

#' @describeIn DPMechGaussian automatically prints the object.
#' @param object an instance of class \code{DPMech}.
setMethod("show", "DPMechGaussian", function(object) {
  cat("Gaussian mechanism\n")
  callNextMethod()
})

#' @describeIn DPMechGaussian releases Gaussian mechanism responses.
#' @param mechanism an object of class \code{\link{DPMechGaussian}}.
#' @param privacyParams an object of class \code{\link{DPParamsDel}}.
#' @param X a privacy-sensitive dataset, if using sensitivity sampler a: list,
#'   matrix, data frame, numeric/character vector.
#' @return list with slots per argument, actual privacy parameter; Gaussian
#'   mechanism response with length of target release:
#'   \code{privacyParams, sensitivity, dims, target, response}.
#' @examples
#' f <- function(xs) mean(xs)
#' n <- 100
#' m <- DPMechGaussian(sensitivity = 1/n, target = f, dims = 1)
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
    if (is.na(mechanism@dims)) {
      warning("No expected non-private dims slot set.")
    }
    if (length(rawR) != mechanism@dims) {
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
      dims = mechanism@dims,
      target = mechanism@target,
      response = R
    ))
  }
)

#' \code{DPMechGaussian} response space L2 norm.
#'
#' Represents the L2 norm on \code{target} responses. For internal use.
#'
#' @param object an instance of class \code{\link{DPMechGaussian-class}}.
#' @param rawR1 a non-private response from \code{target}.
#' @param rawR2 a non-private response from \code{target}.
#' @return a non-negative scalar L2 norm between \code{rawR1}, \code{rawR2}.
setMethod(".numericNorm",
  signature(object = "DPMechGaussian",
            rawR1 = "numeric",
            rawR2 = "numeric"),
  function(object, rawR1, rawR2) {
    return(.l2norm(rawR1 - rawR2))
  }
)
