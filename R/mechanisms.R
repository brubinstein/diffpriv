#' @include privacy_params.R utils.R
NULL

#' An S4 class for differentially-private mechanisms.
#'
#' A base class for representing output-perturbating mechanisms in differential
#' privacy. As this class is \code{VIRTUAL} it cannot be instantiated, but it can
#' be subclassed.
#'
#' @slot sensitivity non-negative scalar numeric target sensitivity.
#' @slot target the target non-private function to be privatized, takes lists.
#' @slot gammaSensitivity \code{NA_real_} if deactive, or scalar in [0,1)
#'   indicating that responses must be RDP with specific confidence.
#'
#' @references Cynthia Dwork, Frank McSherry, Kobbi Nissim, and Adam Smith.
#'   "Calibrating noise to sensitivity in private data analysis." In Theory of
#'   Cryptography Conference, pp. 265-284. Springer Berlin Heidelberg, 2006.
#'
#' @seealso \code{\link{DPMechLaplace}} subclass for the Laplace mechanism.
#'
#' @export
setClass("DPMech",
  slots = list(
    sensitivity = "numeric",
    target = "function",
    gammaSensitivity = "numeric"),
  prototype = prototype(
    sensitivity = Inf,
    target = .constant_target,
    gammaSensitivity = NA_real_),
  contains = "VIRTUAL"
)

## A \code{DPMech} should be constructed with an appropriate sensitivity.
setValidity("DPMech", function(object) {
  if (length(object@sensitivity) != 1) {
    return("DPMech@sensitivity should be a scalar.")
  }
  if (object@sensitivity < 0) {
    return("DPMech@sensitivity should be non-negative.")
  }
  if (!is.numeric(object@gammaSensitivity) ||
      length(object@gammaSensitivity) != 1) {
    return("DPMech@gammaSensitivity should be scalar numeric.")
  }
  if (!is.na(object@gammaSensitivity) &&
      (object@gammaSensitivity < 0 || object@gammaSensitivity >= 1)) {
    return("DPMech@gammaSensitivity should be NA_real_ or in [0,1).")
  }
  return(TRUE)
})

#' \code{DPMech} private release method.
#'
#' Runs the differentially-private mechanism on given data.
#'
#' @param mechanism an object of class \code{\link{DPMech-class}}.
#' @param privacyParams an object of class \code{\link{DPParamsEps}} or subclass.
#' @param X a privacy-sensitive dataset.
#' @return list with slots per argument, including at least: actual privacy
#'   parameters \code{privacyParams}, and response \code{response}.
setGeneric("releaseResponse", function(mechanism, privacyParams, X) {
  standardGeneric("releaseResponse")
})

#' \code{DPMech} sensitivity-inducing norm.
#'
#' Norm of a \code{\link{DPMech-class}}'s non-private \code{target} function
#' evaluated on two given databases \code{X1}, \code{X2}.
#'
#' @param mechanism an object of class \code{\link{DPMech-class}}.
#' @param X1 a privacy-sensitive dataset.
#' @param X2 a privacy-sensitive dataset.
setGeneric("sensitivityNorm", function(mechanism, X1, X2) {
  standardGeneric("sensitivityNorm")
})
