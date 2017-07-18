#' @include mechanisms.R numeric_mechanism.R privacy_params.R utils.R
NULL

#' An S4 class for the Laplace mechanism of differential privacy.
#'
#' A class that implements the basic Laplace mechanism of differential privacy,
#' for privatizing numeric vector releases.
#'
#' @slot sensitivity non-negative scalar numeric L1 target sensitivity.
#'   Defaults to \code{Inf} for use with \code{sensitivitySampler()}.
#' @slot target the target non-private function to be privatized, takes lists.
#'   Defaults to a constant function. Laplace mechanism assumes functions that
#'   release numeric vectors of fixed dimension \code{dims}.
#' @slot gammaSensitivity \code{NA_real_} if inactive, or scalar in [0,1)
#'   indicating that responses must be RDP with specific confidence.
#' @slot dims positive scalar numeric dimension of responses. Defaults to
#'   \code{NA_integer_} for use with \code{sensitivitySampler()} which can
#'   probe \code{target} to determine dimension.
#'
#' @references Cynthia Dwork, Frank McSherry, Kobbi Nissim, and Adam Smith.
#'   "Calibrating noise to sensitivity in private data analysis." In Theory of
#'   Cryptography Conference, pp. 265-284. Springer Berlin Heidelberg, 2006.
#'
#' @export DPMechLaplace
#' @exportClass DPMechLaplace
DPMechLaplace <- setClass("DPMechLaplace",
  contains = "DPMechNumeric"
)

#' @describeIn DPMechLaplace automatically prints the object.
#' @param object an instance of class \code{DPMech}.
setMethod("show", "DPMechLaplace", function(object) {
  cat("Laplace mechanism\n")
  callNextMethod()
})

#' \code{DPMechLaplace} response space L1 norm.
#'
#' Represents the L1 norm on \code{target} responses. For internal use.
#'
#' @param object an instance of class \code{\link{DPMechLaplace-class}}.
#' @param rawR1 a non-private response from \code{target}.
#' @param rawR2 a non-private response from \code{target}.
#' @return a non-negative scalar L1 norm between \code{rawR1}, \code{rawR2}.
setMethod(".numericNorm",
  signature(object = "DPMechLaplace",
            rawR1 = "numeric",
            rawR2 = "numeric"),
  function(object, rawR1, rawR2) {
    return(.l1norm(rawR1 - rawR2))
  }
)

#' \code{DPMechLaplace} release response core.
#'
#' Implements the core calculation specific to the \code{DPMechLaplace}
#' subclass \code{releaseResponse()}. Internal function, not to be called.
#'
#' @param object an instance of class \code{\link{DPMechLaplace-class}}.
#' @param rawR a non-private response from \code{target}.
#' @param privacyParams object of type \code{DPParamsEps}.
#' @return a numeric private response.
setMethod(".responseCore",
  signature(object = "DPMechLaplace",
            rawR = "numeric",
            privacyParams = "DPParamsEps"),
  function(object, rawR, privacyParams) {
    noise <- .rlap(length(rawR),
                   location = 0,
                   scale = object@sensitivity / privacyParams@epsilon)
    R <- rawR + noise
    return(R)
  }
)
