

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
    return(pred)
  } else {
    if(!is.null(object$formula)){
      if(!is.data.frame(newdata)){
  stop("newdata must be empty or a data frame")
}
      modmat <- model.matrix(object, data=newdata)
        } else {
      modmat <- newdata
    }
  
  coefs <- c(coef(object)[1], coef(object)[2] * object$weighting)
  res <- t(t(modmat) * coefs)
  res.total <- rowSums(res)

  if(linkinverse){
  pred <- object$family$linkinv(res.total)} else {
    pred <- res.total
  }
  }
  pred
}


#' @export

predict.iglm <- function(object, newdata = NULL, linkinverse = T) {

  if(is.null(newdata)) {
    pred <- fitted(object)
    return(pred)
  } else {
    if(!is.null(object$formula)){
      if(!is.data.frame(newdata)){
  stop("newdata must be empty or a data frame")
}
      modmat <- model.matrix(object, data=newdata)
        } else {
      modmat <- newdata
    }
  
  coefs <- c(coef(object)[1], coef(object)[2] * object$weighting)
  res <- t(t(modmat) * coefs)
  res.total <- rowSums(res)

  if(linkinverse){
  pred <- object$family$linkinv(res.total)} else {
    pred <- res.total
  }
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


