#' @include mechanisms.R privacy_params.R utils.R bernstein_polynomials.R
#' @importFrom stats predict
NULL

#' An S4 class for the Bernstein mechanism of differential privacy.
#'
#' A class that implements the Bernstein mechanism (not iterated version) of
#' differential privacy, for privatizing release of real-valued functions on
#' \eqn{\[0,1\]^{\ell}}#' based on arbitrary datasets. Approximates the
#' \code{target} on a lattice.
#'
#' @slot sensitivity non-negative scalar numeric maximum absolute \code{target}
#'   sensitivity maximized over the lattice. Defaults to \code{Inf} for use
#'   with \code{sensitivitySampler()}.
#' @slot target might be a closure that takes arbitrary dataset and returns a
#'   real-valued function on \eqn{\[0,1\]^{\ell}}.
#' @slot gammaSensitivity \code{NA_real_} if deactive, or scalar in [0,1)
#'   indicating that responses must be RDP with specific confidence.
#' @slot latticeK positive scalar integer-valued numeric specifying the lattice
#'   resolution. Defaults to (invalid) \code{NA_integer_}.
#' @slot dims positive scalar integer-valued numeric specifying the dimension
#'   of released function domain. Defaults to (invalid) \code{NA_integer_}.
#'
#' @references
#' Francesco Alda and Benjamin I. P. Rubinstein. "The Bernstein Mechanism:
#'   Function Release under Differential Privacy", in Proceedings of the 31st
#'   AAAI Conference on Artificial Intelligence (AAAI'2017), pp. 1705-1711,
#'   Feb 2017.
#'
#' @export DPMechBernstein
#' @exportClass DPMechBernstein
DPMechBernstein <- setClass("DPMechBernstein",
  contains = "DPMech",
  slots = list(latticeK="numeric", dims="numeric"),
  prototype = prototype(latticeK=NA_integer_, dims=NA_integer_)
)

## A \code{DPMechBernstein} should be constructed with appropriate
## lattice parameter, function domain dimension.
setValidity("DPMechBernstein", function(object) {
  if (!.check_integer(object@latticeK) || object@latticeK < 1) {
    return("DPMechBernstein@latticeK should be positive integer-valued.")
  }
  if (!.check_integer(object@dims) || object@dims < 1) {
    return("DPMechBernstein@dims should be positive integer-valued.")
  }
  return(TRUE)
})

#' @describeIn DPMechBernstein automatically prints the object.
#' @param object an instance of class \code{DPMech}.
setMethod("show", "DPMechBernstein", function(object) {
  cat("Bernstein mechanism\n")
  cat("Sensitivity:", object@sensitivity, "\n")
  if (is.na(object@gammaSensitivity)) {
    cat("Sampled sensitivity gamma: NA\n")
  } else {
    cat("Sampled sensitivity gamma:", object@gammaSensitivity, "\n")
  }
  cat("Lattice resolution:", object@latticeK, "\n")
  cat("Function domain dimension:", object@dims, "\n")
  cat("Target function: \n")
  show(object@target)
})

#' @describeIn DPMechBernstein releases Bernstein mechanism responses.
#' @param mechanism an object of class \code{\link{DPMechBernstein}}.
#' @param privacyParams an object of class \code{\link{DPParamsEps}}.
#' @param X a privacy-sensitive dataset, if using sensitivity sampler a: list,
#'   matrix, data frame, numeric/character vector.
#' @return list with slots per argument, actual privacy parameter and response:
#'   mechanism response with length of target release:
#'   \code{privacyParams, sensitivity, latticeK, dims, target, response}.
#' @export
setMethod("releaseResponse",
  signature(mechanism = "DPMechBernstein",
            privacyParams = "DPParamsEps",
            X = "ANY"),
  function(mechanism, privacyParams, X) {
    ## Retrieve non-private function using data X, Bernstein fit
    rawFunc <- mechanism@target(X)
    if (!is.function(rawFunc)) {
      stop("Non-private target output is not a function.")
    }
    appFunc <- bernstein(rawFunc, dims=mechanism@dims, k=mechanism@latticeK)
    ## Laplace-perturb coefficients
    scale <- mechanism@sensitivity *
      (mechanism@latticeK + 1)^mechanism@dims /
      privacyParams@epsilon
    noise <- .rlap((mechanism@latticeK + 1)^(mechanism@dims),
                   location = 0,
                   scale = scale)
    appFunc$coeffs <- appFunc$coeffs + noise
    ## Wrap up results, return
    R <- function(y) predict(appFunc, y)
    if (is.na(mechanism@gammaSensitivity)) {
      p <- privacyParams
    } else {
      p <- toGamma(privacyParams, mechanism@gammaSensitivity)
    }
    return(list(
      privacyParams = p,
      sensitivity = mechanism@sensitivity,
      latticeK = mechanism@latticeK,
      dims = mechanism@dims,
      target = mechanism@target,
      response = R
    ))
  }
)

#' @describeIn DPMechBernstein measures \code{target} sensitivity.
#' @param X1 a privacy-sensitive dataset.
#' @param X2 a privacy-sensitive dataset.
#' @return scalar numeric norm of non-private \code{target} on datasets.
#'   The \eqn{L_\infty} of the functions on a lattice.
#' @export
setMethod("sensitivityNorm",
  signature(mechanism = "DPMechBernstein",
            X1 = "ANY",
            X2 = "ANY"),
  function(mechanism, X1, X2) {
    ## The raw functions to start
    rawFunc1 <- mechanism@target(X1)
    rawFunc2 <- mechanism@target(X2)
    if (!is.function(rawFunc1) || !is.function(rawFunc2)) {
      stop("Non-private target output is not a function.")
    }
    ## Lattice on which to compare
    latt <- .bernstein_lattice(
      d = mechanism@dims,
      k = mechanism@latticeK) / mechanism@latticeK
    ## Compare functions as their max deviation on the lattice
    rs1 <- apply(latt, 1, rawFunc1)
    rs2 <- apply(latt, 1, rawFunc2)
    return(.linfty_norm(rs1 - rs2))
  }
)
