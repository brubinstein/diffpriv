#' @include mechanisms.R privacy_params.R utils.R
NULL

#' Sensitivity sampler for \code{\link{DPMech-class}}.
#'
#' Given a constructed \code{\link{DPMech-class}}, complete with \code{target}
#' function and \code{sensitivityNorm,} and an \code{oracle} for producing
#' records, samples the sensitivity of the target function to set the
#' mechanism's \code{sensitivity}. Typically the method
#' \code{\link{sensitivitySampler}} should be used instead; NOTE this method
#' does not properly set the \code{gammaSensitivity} slot of
#' \code{\link{DPMech-class}} unlike the preferred method.
#'
#' @param object an object of class \code{\link{DPMech-class}}.
#' @param oracle a source of random databases. A function returning: list,
#'   matrix/data.frame (data in rows), numeric/character vector of records if
#'   given desired length > 1; or single record given length 1, respectively
#'   a list element, a row/named row,  a single numeric/character. Whichever
#'   type is used should be expected by \code{object@target}.
#' @param n database size scalar positive numeric, integer-valued.
#' @param m sensitivity sample size scalar positive numeric, integer-valued.
#' @param k order statistic index in {1,...,\code{m}}.
#' @return \code{object} with updated sensitivity parameter.
#'
#' @references Benjamin I. P. Rubinstein and Francesco Aldà. "Pain-Free Random Differential
#'   Privacy with Sensitivity Sampling", accepted into the 34th International
#'   Conference on Machine Learning (ICML'2017), May 2017.
#'
#' @examples
#' ## Simple example with unbounded data hence no global sensitivity.
#' f <- function(xs) mean(xs)
#' m <- DPMechLaplace(target = f, dims = 1)
#' P <- function(n) rnorm(n)
#' m <- sensitivitySamplerManual(m, oracle = P, n = 100, m = 10, k = 10)
#' m@sensitivity
#'
#' @seealso \code{\link{sensitivitySampler}} preferred method for sensitivity
#'   sampling.
#'
#' @export
setGeneric("sensitivitySamplerManual",
  function(object, oracle, n, m, k) {
  standardGeneric("sensitivitySamplerManual")
})

#' Sensitivity sampler for \code{\link{DPMech-class}}.
#'
#' Given a constructed \code{\link{DPMech-class}}, complete with \code{target}
#' function and \code{sensitivityNorm,} and an \code{oracle} for producing
#' records, samples the sensitivity of the target function to set the
#' mechanism's \code{sensitivity}. Typically the method
#' \code{\link{sensitivitySampler}} should be used instead; NOTE this method
#' does not properly set the \code{gammaSensitivity} slot of
#' \code{\link{DPMech-class}} unlike the preferred method.
#'
#' @param object an object of class \code{\link{DPMech-class}}.
#' @param oracle a source of random databases. A function returning: list,
#'   matrix/data.frame (data in rows), numeric/character vector of records if
#'   given desired length > 1; or single record given length 1, respectively
#'   a list element, a row/named row,  a single numeric/character. Whichever
#'   type is used should be expected by \code{object@target}.
#' @param n database size scalar positive numeric, integer-valued.
#' @param m sensitivity sample size scalar positive numeric, integer-valued.
#' @param k order statistic index in {1,...,\code{m}}.
#' @return \code{object} with updated sensitivity parameter.
#'
#' @references
#' Benjamin I. P. Rubinstein and Francesco Aldà. "Pain-Free Random Differential
#'   Privacy with Sensitivity Sampling", accepted into the 34th International
#'   Conference on Machine Learning (ICML'2017), May 2017.
#'
#' @examples
#' ## Simple example with unbounded data hence no global sensitivity.
#' f <- function(xs) mean(xs)
#' m <- DPMechLaplace(target = f, dims = 1)
#' P <- function(n) rnorm(n)
#' m <- sensitivitySamplerManual(m, oracle = P, n = 100, m = 10, k = 10)
#' m@sensitivity
#'
#' @seealso \code{\link{sensitivitySampler}} preferred method for sensitivity
#'   sampling.
#'
#' @export
setMethod("sensitivitySamplerManual",
  signature(object = "DPMech",
            oracle = "function",
            n = "numeric",
            m = "numeric",
            k = "numeric"),
  function(object, oracle, n, m, k) {
    if (!.check_integer(n) || n < 1)
      stop("Given database size n is not positive scalar integer-valued.")
    if (!.check_integer(m) || m < 1)
      stop("Given sample size m is not positive scalar integer-valued.")
    if (!.check_integer(k))
      stop("Given order statistic index k is not scalar integer-valued.")
    if (k <= 0 || k > m)
      stop("Given order statistic index k is not in {1,...,m}.")
    Gs <- rep(Inf, m)
    for (i in 1:m) {
      db1 <- oracle(n-1)
      db2 <- db1
      db1 <- .generic_append(db1, oracle(1))
      db2 <- .generic_append(db2, oracle(1))
      Gs[i] <- sensitivityNorm(object, db1, db2)
    }
    Gs <- sort(Gs, decreasing = FALSE)
    object@sensitivity <- Gs[k]
    return(object)
  }
)

#' Sensitivity sampler parameter optimization.
#'
#' Internal function for optimizing internal variable \code{rho}.
#' Either but not both inputs can be omitted as \code{NA_integer_}
#' or \code{NA_real_} respectively.
#'
#' @param m sensitivity sample size.
#' @param gamma the RDP privacy confidence level.
#' @param supressCheck Whether or not to check for min \code{gamma}.
#' @return Returns the configuration parameters:
#' \itemize{
#'   \item \code{m}: Sensitivity sample size.
#'   \item \code{gamma}: The RDP privacy confidence level.
#'   \item \code{k}: The order statistic index.
#'   \item \code{rho}: Internal DKW inequality parameter.
#'     Used in computing others.
#'   \item \code{mode}: char string specifying the parameter optimised.
#' }
#'
#' @references
#' Benjamin I. P. Rubinstein and Francesco Aldà. "Pain-Free Random Differential
#'   Privacy with Sensitivity Sampling", accepted into the 34th International
#'   Conference on Machine Learning (ICML'2017), May 2017.
.sensitivity_sampler_config <- function(m = NA_integer_, gamma = NA_real_,
  suppressCheck = FALSE) {
  if (is.na(m) && is.na(gamma))
    stop("Must specify at least one input m or gamma.")
  if (!is.na(m)) {
    if (!.check_integer(m) || m < 1)
      stop("Given m is not scalar positive integer.")
  }
  if (!is.na(gamma)) {
    if (!is.numeric(gamma) || length(gamma) != 1 || gamma <= 0 || gamma >= 1)
      stop("Given gamma is not scalar numeric in (0,1).")
  }
  if (!is.na(m)) {
    # Given m
    rho <- exp(gsl::lambert_Wm1(-1 / (4 * m)) / 2)
    gamma.lo <- rho + sqrt(log(1 / rho) / (2 * m))
    if (!is.na(gamma)) {
      # Given gamma; Optimise k
      mode <- "k"
      if (!suppressCheck) {
        if (gamma < gamma.lo)
          stop("Given gamma ", gamma, " must exceed ", gamma.lo)
      }
      k <- ceiling(m * (1 - gamma + gamma.lo))
    } else {
      # Not given gamma; Optimise gamma
      mode <- "gamma"
      gamma <- gamma.lo
      k <- m
    }
  } else {
    # Given gamma not m; Optimise m
    mode <- "m"
    rho <- exp(gsl::lambert_Wm1(-gamma / (2 * exp(0.5))) + 0.5)
    m <- ceiling(log(1 / rho) / (2 * (gamma - rho)^2))
    gamma.lo <- rho + sqrt(log(1 / rho) / (2 * m))
    k <- ceiling(m * (1 - gamma + gamma.lo))
  }
  return(list(m = m, gamma = gamma, k = k, rho = rho, mode = mode))
}

#' Sensitivity sampler for \code{\link{DPMech-class}}'s.
#'
#' Given a constructed \code{\link{DPMech-class}}, complete with \code{target}
#' function and \code{sensitivityNorm,} and an \code{oracle} for producing
#' records, samples the sensitivity of the target function to set the
#' mechanism's \code{sensitivity}.
#'
#' @param object an object of class \code{\link{DPMech-class}}.
#' @param oracle a source of random databases. A function returning: list,
#'   matrix/data.frame (data in rows), numeric/character vector of records if
#'   given desired length > 1; or single record given length 1, respectively
#'   a list element, a row/named row,  a single numeric/character. Whichever
#'   type is used should be expected by \code{object@target}.
#' @param n database size scalar positive numeric, integer-valued.
#' @param m sensitivity sample size scalar positive numeric, integer-valued.
#' @param gamma RDP privacy confidence level.
#' @return \code{object} with updated \code{gammaSensitivity} slot.
#'
#' @references
#' Benjamin I. P. Rubinstein and Francesco Aldà. "Pain-Free Random Differential
#'   Privacy with Sensitivity Sampling", accepted into the 34th International
#'   Conference on Machine Learning (ICML'2017), May 2017.
#'
#' @examples
#' ## Simple example with unbounded data hence no global sensitivity.
#' f <- function(xs) mean(xs)
#' m <- DPMechLaplace(target = f, dims = 1)
#' m@sensitivity ## Inf
#' m@gammaSensitivity ## NA as Laplace is naturally eps-DP
#' P <- function(n) rnorm(n)
#' m <- sensitivitySampler(m, oracle = P, n = 100, gamma = 0.33)
#' m@sensitivity ## small like 0.03...
#' m@gammaSensitivity ## 0.33 as directed, now m is (eps,gam)-DP.
#'
#' @export
setGeneric("sensitivitySampler",
  function(object, oracle, n, m = NA_integer_, gamma = NA_real_) {
    standardGeneric("sensitivitySampler")
})

#' Sensitivity sampler for \code{\link{DPMech-class}}'s.
#'
#' Given a constructed \code{\link{DPMech-class}}, complete with \code{target}
#' function and \code{sensitivityNorm,} and an \code{oracle} for producing
#' records, samples the sensitivity of the target function to set the
#' mechanism's \code{sensitivity}.
#'
#' @param object an object of class \code{\link{DPMech-class}}.
#' @param oracle a source of random databases. A function returning: list,
#'   matrix/data.frame (data in rows), numeric/character vector of records if
#'   given desired length > 1; or single record given length 1, respectively
#'   a list element, a row/named row,  a single numeric/character. Whichever
#'   type is used should be expected by \code{object@target}.
#' @param n database size scalar positive numeric, integer-valued.
#' @param m sensitivity sample size scalar positive numeric, integer-valued.
#' @param gamma RDP privacy confidence level.
#' @return \code{object} with updated \code{gammaSensitivity} slot.
#'
#' @references
#' Benjamin I. P. Rubinstein and Francesco Aldà. "Pain-Free Random Differential
#'   Privacy with Sensitivity Sampling", accepted into the 34th International
#'   Conference on Machine Learning (ICML'2017), May 2017.
#'
#' @examples
#' ## Simple example with unbounded data hence no global sensitivity.
#' f <- function(xs) mean(xs)
#' m <- DPMechLaplace(target = f, dims = 1)
#' m@sensitivity ## Inf
#' m@gammaSensitivity ## NA as Laplace is naturally eps-DP
#' P <- function(n) rnorm(n)
#' m <- sensitivitySampler(m, oracle = P, n = 100, gamma = 0.33)
#' m@sensitivity ## small like 0.03...
#' m@gammaSensitivity ## 0.33 as directed, now m is (eps,gam)-DP.
#'
#' @export
setMethod("sensitivitySampler",
  signature(object = "DPMech",
            oracle = "function",
            n = "numeric"),
  function(object, oracle, n, m = NA_integer_, gamma = NA_real_) {
    ssconfig <- .sensitivity_sampler_config(m = m, gamma = gamma)
    object@gammaSensitivity <- ssconfig$gamma
    message("Sampling sensitivity with",
            " m=", ssconfig$m,
            " gamma=", ssconfig$gamma,
            " k=", ssconfig$k)
    return(sensitivitySamplerManual(
      object = object,
      oracle = oracle,
      n = n,
      m = ssconfig$m,
      k = ssconfig$k))
  }
)

#' Sensitivity sampler for \code{\link{DPMechNumeric-class}}.
#'
#' Given a constructed \code{\link{DPMechNumeric-class}}, complete with
#' \code{target} function and \code{sensitivityNorm,} and an \code{oracle} for
#' producing records, samples the sensitivity of the target function to set the
#' mechanism's \code{sensitivity}. Typically the method
#' \code{\link{sensitivitySampler}} should be used instead; NOTE this method
#' does not properly set the \code{gammaSensitivity} slot of
#' \code{\link{DPMech-class}} unlike the preferred method. This method can
#' probe \code{target} to determine response dimension when the
#' corresponding \code{object@dims} is \code{NA}.
#'
#' @param object an object of class \code{\link{DPMechNumeric-class}}.
#' @param oracle a source of random databases. A function returning: list,
#'   matrix/data.frame (data in rows), numeric/character vector of records if
#'   given desired length > 1; or single record given length 1, respectively
#'   a list element, a row/named row,  a single numeric/character. Whichever
#'   type is used should be expected by \code{object@target}.
#' @param n database size scalar positive numeric, integer-valued.
#' @param m sensitivity sample size scalar positive numeric, integer-valued.
#' @param k order statistic index in {1,...,\code{m}}.
#' @return \code{object} with updated sensitivity parameter, and (potentially)
#'   \code{dims}.
#'
#' @references
#' Benjamin I. P. Rubinstein and Francesco Aldà. "Pain-Free Random Differential
#'   Privacy with Sensitivity Sampling", accepted into the 34th International
#'   Conference on Machine Learning (ICML'2017), May 2017.
#'
#' @examples
#' ## Simple example with unbounded data hence no global sensitivity.
#' f <- function(xs) mean(xs)
#' m <- DPMechLaplace(target = f, dims = 1)
#' P <- function(n) rnorm(n)
#' m <- sensitivitySamplerManual(m, oracle = P, n = 100, m = 10, k = 10)
#' m@sensitivity
#'
#' @seealso \code{\link{sensitivitySampler}} preferred method for sensitivity
#'   sampling.
#'
#' @export
setMethod("sensitivitySamplerManual",
  signature(object = "DPMechNumeric",
            oracle = "function",
            n = "numeric",
            m = "numeric",
            k = "numeric"),
  function(object, oracle, n, m, k) {
    if (!.check_integer(n) || n < 1)
      stop("Given database size n is not positive scalar integer-valued.")
    if (!.check_integer(m) || m < 1)
      stop("Given sample size m is not positive scalar integer-valued.")
    if (!.check_integer(k))
      stop("Given order statistic index k is not scalar integer-valued.")
    if (k <= 0 || k > m)
      stop("Given order statistic index k is not in {1,...,m}.")
    if (is.na(object@dims)) {
      db <- oracle(n)
      R <- object@target(db)
      object@dims <- length(R)
    }
    return(callNextMethod(object=object, oracle=oracle, n=n, m=m, k=k))
  }
)
