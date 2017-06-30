#' An S4 class for basic differential privacy parameters.
#'
#' An S4 base class representing the basic privacy parameter \eqn{\epsilon} in
#' differential privacy.
#'
#' @slot epsilon positive scalar numeric privacy level.
#'
#' @seealso \code{\link{DPParamsDel}} subclass for \eqn{(\epsilon,\delta)}
#'   relaxation, \code{\link{DPParamsGam}} subclass for random relaxation.
#'
#' @export DPParamsEps
#' @exportClass DPParamsEps
DPParamsEps <- setClass("DPParamsEps",
  slots = list(epsilon = "numeric"),
  prototype = prototype(epsilon = 1.0)
)

## Enforce allowable parameter range for \code{DPParamsEps}.
setValidity("DPParamsEps", function(object) {
  if (length(object@epsilon) != 1) {
    return("DPParamsEps@epsilon should be a scalar.")
  }
  if (object@epsilon <= 0) {
    return("DPParamsEps@epsilon should be positive.")
  }
  return(TRUE)
})

#' @describeIn DPParamsEps automatically prints the object.
setMethod("show", "DPParamsEps", function(object) {
  cat("Differential privacy level \u03B5", object@epsilon, sep = "=")
})

setGeneric("getEpsilon", function(object) {
  standardGeneric("getEpsilon")
})

#' @describeIn DPParamsEps getter for slot \code{epsilon}.
setMethod("getEpsilon", "DPParamsEps", function(object) return(object@epsilon))

#' Setter for slot \code{epsilon}.
#'
#' Use this method instead of slot \code{epsilon}.
#'
#' @param object the instance of \code{DPParamsEps}.
#' @param value positive numeric \eqn{\epsilon} value.
#'
#' @seealso \code{\link{DPParamsEps}}.
setGeneric("setEpsilon<-", function(object, value)
  standardGeneric("setEpsilon<-"))

#' @describeIn DPParamsEps setter for slot \code{epsilon}.
#' @param object an object of class \code{\link{DPParamsEps}}.
#' @param value a scalar numeric \eqn{\epsilon}.
setReplaceMethod("setEpsilon", "DPParamsEps", function(object, value) {
  object@epsilon <- value
  validObject(object)
  return(object)
})

setGeneric("toGamma", function(object, gamma) {
  standardGeneric("toGamma")
})

#' @describeIn DPParamsEps returns object to corresponding instance of subclass
#'   \code{\link{DPParamsGam}}.
#' @param gamma a scalar numeric \eqn{\gamma}.
setMethod("toGamma", signature(object = "DPParamsEps", gamma = "numeric"),
  function(object, gamma) {
    p <- as(object, "DPParamsGam", strict = TRUE)
    setDelta(p) <- 0
    setGamma(p) <- gamma
    return(p)
  }
)

#' An S4 class for relaxed differential privacy parameters.
#'
#' An S4 base class representing the privacy parameters in
#' \eqn{(\epsilon,\delta)}-differential privacy.
#'
#' @slot epsilon positive scalar numeric privacy level.
#' @slot delta a scalar numeric privacy level in interval [0,1).
#'
#' @seealso \code{\link{DPParamsEps}} superclass,
#' \code{\link{DPParamsGam}} subclass for random relaxation.
#'
#' @export DPParamsDel
#' @exportClass DPParamsDel
DPParamsDel <- setClass("DPParamsDel",
  slots = list(delta = "numeric"),
  contains = "DPParamsEps",
  prototype = prototype(epsilon = 1.0, delta=0.0)
)

## Enforce allowable parameter range for \code{DPParamsDel}.
setValidity("DPParamsDel", function(object) {
  if (length(object@delta) != 1) {
    return("DPParamsDel@delta should be a scalar.")
  }
  if (object@delta < 0 || object@delta >= 1) {
    return("DPParamsDel@delta should be in [0,1).")
  }
  return(TRUE)
})

#' @describeIn DPParamsDel automatically prints the object.
setMethod("show", "DPParamsDel", function(object) {
  cat("Differential privacy level ",
      "\u03B5=", object@epsilon,
      ", \u03B4=", object@delta, sep="")
})

setGeneric("getDelta", function(object) {
  standardGeneric("getDelta")
})

#' @describeIn DPParamsDel getter for slot \code{delta}.
setMethod("getDelta", "DPParamsDel", function(object) return(object@delta))

#' Setter for slot \code{delta}.
#'
#' Use this method instead of slot \code{delta}.
#'
#' @param object the instance of \code{DPParamsDel}.
#' @param value positive numeric \eqn{\delta} value.
#'
#' @seealso \code{\link{DPParamsDel}}.
setGeneric("setDelta<-", function(object, value)
  standardGeneric("setDelta<-"))

#' @describeIn DPParamsDel setter for slot \code{delta}.
#' @param object an object of class \code{\link{DPParamsDel}}.
#' @param value a scalar numeric \eqn{\delta}.
setReplaceMethod("setDelta", "DPParamsDel", function(object, value) {
  object@delta <- value
  validObject(object)
  return(object)
})

#' @describeIn DPParamsDel returns object to corresponding instance of subclass
#'   \code{\link{DPParamsGam}}.
#' @param gamma a scalar numeric \eqn{\gamma}.
setMethod("toGamma", signature(object = "DPParamsDel", gamma = "numeric"),
  function(object, gamma) {
    p <- as(object, "DPParamsGam", strict = TRUE)
    setGamma(p) <- gamma
    return(p)
  }
)

#' An S4 class for random differential privacy parameters.
#'
#' An S4 base class representing the privacy parameters in
#' \eqn{(\epsilon,\delta,\gamma)}-random differential privacy.
#'
#' @slot epsilon positive scalar numeric privacy level.
#' @slot delta a scalar numeric privacy level in interval [0,1).
#' @slot gamma a scalar numeric privacy level in [0, 1).
#'
#' @seealso \code{\link{DPParamsEps}}, \code{\link{DPParamsDel}} superclasses.
#'
#' @export DPParamsGam
#' @exportClass DPParamsGam
DPParamsGam <- setClass("DPParamsGam",
  slots = list(gamma = "numeric"),
  contains = "DPParamsDel",
  prototype = prototype(epsilon = 1.0, delta = 0.0, gamma = 0.05)
)

## Enforce allowable parameter range for \code{DPParamsGam}.
setValidity("DPParamsGam", function(object) {
  if (length(object@gamma) != 1) {
    return("DPParamsGam@gamma should be a scalar.")
  }
  if (object@gamma < 0 || object@gamma >= 1) {
    return("DPParamsGam@gamma should be in [0,1).")
  }
  return(TRUE)
})

#' @describeIn DPParamsGam automatically prints the object.
setMethod("show", "DPParamsGam", function(object) {
  cat("Random differential privacy level ",
      "\u03B5=", object@epsilon,
      ", \u03B4=", object@delta,
      ", \u03B3=", object@gamma, sep="")
})

setGeneric("getGamma", function(object) {
  standardGeneric("getGamma")
})

#' @describeIn DPParamsGam getter for slot \code{gamma}.
setMethod("getGamma", "DPParamsGam", function(object) return(object@gamma))

#' Setter for slot \code{gamma}.
#'
#' Use this method instead of slot \code{gamma}.
#'
#' @param object the instance of \code{DPParamsGam}.
#' @param value positive numeric \eqn{\gamma} value.
#'
#' @seealso \code{\link{DPParamsGam}}.
setGeneric("setGamma<-", function(object, value)
  standardGeneric("setGamma<-"))

#' @describeIn DPParamsGam setter for slot \code{gamma}.
#' @param object an object of class \code{\link{DPParamsGam}}.
#' @param value a scalar numeric \eqn{\gamma}.
setReplaceMethod("setGamma", "DPParamsGam", function(object, value) {
  object@gamma <- value
  validObject(object)
  return(object)
})

#' @describeIn DPParamsGam returns object with set gamma; generic for use with
#'   superclasses \code{\link{DPParamsEps}} and \code{\link{DPParamsDel}}.
#' @param gamma scalar numeric \eqn{\gamma}.
setMethod("toGamma", signature(object = "DPParamsGam", gamma = "numeric"),
  function(object, gamma) {
    warning("Calling toGamma() on DPParamsGam same as setGamma().")
    setGamma(object) <- gamma
    return(object)
  }
)
