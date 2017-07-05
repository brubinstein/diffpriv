#' @include mechanisms.R privacy_params.R utils.R
NULL

#' An S4 class for the exponential mechanism of differential privacy.
#'
#' A class that implements the exponential mechanism of differential privacy,
#' for privatizing releases from sets (not necessarily numeric as
#' required by \code{\link{DPMechLaplace}}). Currently limited to responses
#' from a finite sets - the most widely used case - as these induce easily
#' computed sampling distributions from a uniform base measure.
#'
#' @slot sensitivity non-negative scalar numeric quality function sensitivity.
#'   Defaults to \code{Inf} for use with \code{sensitivitySampler()}.
#' @slot target the quality score function mapping dataset to a function on
#'   responses (elements of \code{responseSet}).
#' @slot gammaSensitivity \code{NA_real_} if deactive, or scalar in [0,1)
#'   indicating that responses must be RDP with specific confidence.
#' @slot responseSet a list of possible responses of the mechanism.
#'
#' @references
#' Frank McSherry and Kunal Talwar. "Mechanism design via differential privacy."
#'   In the 48th Annual IEEE Symposium on Foundations of Computer Science
#'   (FOCS'07), pp. 94-103. IEEE, 2007.
#'
#' @export DPMechExponential
#' @exportClass DPMechExponential
DPMechExponential <- setClass("DPMechExponential",
  contains = "DPMech",
  slots = list(responseSet = "list")
)

## A \code{DPMechExponential} should be constructed with an appropriate
## response set.
setValidity("DPMechExponential", function(object) {
  if (!is.list(object@responseSet) || length(object@responseSet) <= 0) {
    return("DPMechExponential@responseSet should be non-empty list.")
  }
  return(TRUE)
})

#' @describeIn DPMechExponential automatically prints the object.
#' @param object an instance of class \code{DPMech}.
setMethod("show", "DPMechExponential", function(object) {
  cat("Exponential mechanism\n")
  cat("Sensitivity:", object@sensitivity, "\n")
  if (is.na(object@gammaSensitivity)) {
    cat("Sampled sensitivity gamma: NA\n")
  } else {
    cat("Sampled sensitivity gamma:", object@gammaSensitivity, "\n")
  }
  cat("Response set:", paste(object@responseSet))
  cat("Quality score function: \n")
  show(object@target)
})

#' @describeIn DPMechExponential releases exponential mechanism responses.
#' @param mechanism an object of class \code{\link{DPMechExponential}}.
#' @param privacyParams an object of class \code{\link{DPParamsEps}}.
#' @param X a privacy-sensitive dataset, a list if using sensitivity sampler.
#' @return list with slots per argument, actual privacy parameter and response:
#'   mechanism response with length of target release:
#'   \code{privacyParams, sensitivity, responseSet, target, response}.
#' @examples
#' ## Sensitive data are strings of length at most 5.
#' ## Task is to release most frequent character present, hence quality function
#' ## is a closure that counts character frequencies for given candidate char.
#' ## Global sensitivity is max string length.
#' qualF <- function(X) { function(r) sum(r == unlist(strsplit(X, ""))) }
#' rs <- as.list(letters)
#' m <- DPMechExponential(sensitivity = 5, target = qualF, responseSet = rs)
#' X <- strsplit("the quick brown fox jumps over the lazy dog"," ")[[1]]
#' p <- DPParamsEps(epsilon = 1)
#' releaseResponse(m, p, X)
#' @export
setMethod("releaseResponse",
  signature(mechanism = "DPMechExponential",
            privacyParams = "DPParamsEps",
            X = "ANY"),
  function(mechanism, privacyParams, X) {
    scoreFunc <- mechanism@target(X)  ## target returns a function
    qualities <- sapply(mechanism@responseSet, scoreFunc)
    pmf <- qualities * (getEpsilon(privacyParams) / (2*mechanism@sensitivity))
    pmf <- pmf / sum(pmf)
    R <- sample(mechanism@responseSet, size=1, prob=pmf)[[1]]
    if (is.na(mechanism@gammaSensitivity)) {
      p <- privacyParams
    } else {
      p <- toGamma(privacyParams, mechanism@gammaSensitivity)
    }
    return(list(
      privacyParams = p,
      sensitivity = mechanism@sensitivity,
      responseSet = mechanism@responseSet,
      target = mechanism@target,
      response = R
    ))
  }
)

#' @describeIn DPMechExponential measures \code{target} quality score sensitivity.
#' @param X1 a privacy-sensitive dataset, list if sensitivity sampler compatible.
#' @param X2 a privacy-sensitive dataset, list if sensitivity sampler compatible.
#' @return scalar numeric norm of non-private \code{target} on datasets.
#' @export
setMethod("sensitivityNorm",
  signature(mechanism = "DPMechExponential",
            X1 = "ANY",
            X2 = "ANY"),
  function(mechanism, X1, X2) {
    scoreFunc1 <- mechanism@target(X1)
    scoreFunc2 <- mechanism@target(X2)
    if (!is.function(scoreFunc1) || !is.function(scoreFunc2)) {
      stop("Non-private target output is not a functioin.")
    }
    scores1 <- sapply(mechanism@responseSet, scoreFunc1)
    scores2 <- sapply(mechanism@responseSet, scoreFunc2)
    return(.linfty_norm(scores1 - scores2))
  }
)
