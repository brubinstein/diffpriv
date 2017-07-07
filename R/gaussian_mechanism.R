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

#' \code{DPMechGaussian} release response core.
#'
#' Implements the core calculation specific to the \code{DPMechGaussian}
#' subclass \code{releaseResponse()}. Internal function, not to be called.
#'
#' @param object an instance of class \code{\link{DPMechGaussian-class}}.
#' @param rawR a non-private response from \code{target}.
#' @param privacyParams object of type \code{DPParamsDel}.
#' @return a numeric private response.
setMethod(".responseCore",
  signature(object = "DPMechGaussian",
            rawR = "numeric",
            privacyParams = "DPParamsDel"),
  function(object, rawR, privacyParams) {
    C <- sqrt(2 * log(1.25 / privacyParams@delta))
    noise <- stats::rnorm(length(rawR), mean = 0,
                          sd = object@sensitivity * C / privacyParams@epsilon)
    R <- rawR + noise
    return(R)
  }
)
