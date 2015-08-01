

#' Extract variable weights from ilm or iglm object
#'
#' @param object an \code{ilm} or \code{iglm} object.
#'
#' @seealso \code{\link{ilm}}, \code{\link{iglm}}
#'
#' @export

weighting <- function(object) object$weighting


#' @export

predict.ilm <- function(object, newdata = NULL) {

  if(is.null(newdata)) {
    pred <- fitted(object)
  } else {
    if(!is.null(object$formula)){
      x <- model.matrix(object, newdata)
    } else {
      x <- newdata
    }

    intercept <- attr(x, "assign") == 0
    if (any(intercept))
      x <- x[, !intercept, drop = FALSE]

    x <- scale(x, center = if (!is.null(object$scaling$center))
                             object$scaling$center else FALSE,
                  scale = if (!is.null(object$scaling$scale))
                             object$scaling$scale else FALSE)

    variate <- as.vector(x %*% object$weighting)

    if (length(coef(object)) == 2)
      fit <- coef(object)[1] + coef(object)[2] * variate
    else
      fit <- coef(object)[1] * variate

    pred <- fit
  }
  pred

}


#' @export

predict.iglm <- function(object, newdata = NULL) {

  if(is.null(newdata)) {
    pred <- fitted(object)
  } else {
    if(!is.null(object$formula)){
      x <- model.matrix(object, newdata)
    } else {
      x <- newdata
    }

    intercept <- attr(x, "assign") == 0
    if (any(intercept))
      x <- x[, !intercept, drop = FALSE]

    x <- scale(x, center = if (!is.null(object$scaling$center))
                             object$scaling$center else FALSE,
                  scale = if (!is.null(object$scaling$scale))
                             object$scaling$scale else FALSE)

    variate <- as.vector(x %*% object$weighting)

    if (length(coef(object)) == 2)
      fit <- coef(object)[1] + coef(object)[2] * variate
    else
      fit <- coef(object)[1] * variate

    pred <- object$family$linkinv(fit)
  }
  pred

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


