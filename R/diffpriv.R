#' \code{diffpriv}: easy differential privacy in R.
#'
#' The \code{diffpriv} package is a collection of generic tools for privacy-aware
#' data science, under the formal framework of differential privacy. A
#' differentially-private mechanism can release responses to untrusted third
#' parties, models fit on privacy-sensitive data. Due to the formal worst-case
#' nature of the framework, however, mechanism development typically requires
#' theoretical analysis. \code{diffpriv} offers a turn-key approach to
#' differential privacy.
#'
#' @section General-purpose mechanisms:
#'
#' Differential privacy's popularity is owed in part to a number of generic
#' mechanisms for privatizing non-private target functions. Virtual S4
#' class \code{\link{DPMech-class}} captures common features of these mechanisms and
#' is superclass to:
#'
#' \itemize{
#'   \item \code{\link{DPMechLaplace}}: the Laplace mechanism of Dwork et al.
#'     (2006) for releasing numeric vectors;
#'   \item \code{\link{DPMechExponential}}: the exponential mechanism of McSherry
#'     and Talwar (2007) for releasing solutions to optimizations, over numeric or
#'     non-numeric sets; and
#'   \item More mechanisms coming soon. Users can also develop new mechanisms by
#'     subclassing \code{\link{DPMech-class}}.
#' }
#'
#' \code{\link{DPMech-class}}-derived objects are initialized with a problem-specific
#' non-private \code{target} function. Subsequently, the
#' \code{\link{releaseResponse}} method can privatize responses of \code{target}
#' on input datasets. The level of correpsonding privatization depends on given
#' privacy parameters \code{\link{DPParamsEps}} or derived parameters object.
#'
#' @section Privatize anything with sensitivity measurement:
#'
#' \code{diffpriv} mechanisms have in common a reliance on the 'sensitivity' of
#' \code{target} function to small changes to input datasets. This sensitivity
#' must be provably bounded for an application's \code{target} in order for
#' differential privacy to be proved, and is used to calibrate privacy-preserving
#' randomization. Unfortunately bounding sensitivity is often prohibitively
#' complex, for example if \code{target} is an arbitrary computer program. All
#' \code{\link{DPMech-class}} mechanisms offer a \code{\link{sensitivitySampler}}
#' method due to Rubinstein and Aldà (2017) that repeatedly probes \code{target}
#' to estimate sensitivity automatically. Mechanisms with estimated sensitivities
#' achieve a slightly weaker form of random differential privacy due to
#' Hall et al. (2013), but without any theoretical analysis necessary.
#'
#' @examples
#' \dontrun{
#' ## for full examples see the diffpriv vignette
#' vignette("diffpriv")
#' }
#'
#' @references
#' Benjamin I. P. Rubinstein and Francesco Aldà. "Pain-Free Random Differential
#'   Privacy with Sensitivity Sampling", accepted into the 34th International
#'   Conference on Machine Learning (ICML'2017), May 2017.
#'
#' Cynthia Dwork, Frank McSherry, Kobbi Nissim, and Adam Smith.
#'   "Calibrating noise to sensitivity in private data analysis." In Theory of
#'   Cryptography Conference, pp. 265-284. Springer Berlin Heidelberg, 2006.
#'
#' Frank McSherry and Kunal Talwar. "Mechanism design via differential privacy."
#'   In the 48th Annual IEEE Symposium on Foundations of Computer Science
#'   (FOCS'07), pp. 94-103. IEEE, 2007.
#'
#' Rob Hall, Alessandro Rinaldo, and Larry Wasserman. "Random Differential
#'   Privacy." Journal of Privacy and Confidentiality, 4(2), pp. 43-59, 2012.
#'
#' @docType package
#' @name diffpriv
#' @import methods
NULL
