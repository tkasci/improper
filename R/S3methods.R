


#' Extract variable weights from ilm or iglm object
#'
#' @param object an \code{ilm} or \code{iglm} object.
#'
#' @seealso \code{\link{ilm}}, \code{\link{iglm}}
#'
#' @export

weighting <- function(object)
  object$weighting


#' @export

predict.ilm <- function(object, newdata = NULL) {
  if (is.null(newdata)) {
    return(object$fitted.values)
  }
  intercept <- coef(object)[1]
  variate <- coef(object)[2]
  varnames <- names(object$weighting)
  varweight <- variate * object$weighting
  data.select <- newdata[, names(object$weighting)]
  res <- coef(object)[1] + rowSums(varweight * data.select)
  res
}


#' @export

predict.iglm <- function(object, newdata = NULL) {
  if (is.null(newdata)) {
    return(object$fitted.values)
  }
  intercept <- coef(object)[1]
  variate <- coef(object)[2]
  varnames <- names(object$weighting)
  varweight <- variate * object$weighting
  data.select <- newdata[, names(object$weighting)]
  res <- coef(object)[1] + rowSums(varweight * data.select)
  res
}

#' @export

print.ilm <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
  cat("\nWeights:\n")
  print(x$weighting)
}


#' @export

summary.ilm <- function(object, ...) {
  print(summary.lm(object, ...))
  cat("Weights:\n")
  print(object$weighting)
}


#' @export

summary.iglm <- function(object, ...) {
  print(summary.glm(object, ...))
  cat("Weights:\n")
  print(object$weighting)
}
