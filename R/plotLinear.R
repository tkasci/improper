

#' Plot (improper) weighted linear combinations of variables
#'
#' @param formula   an object of class \code{"formula"} (or one that can
#'                  be coerced to that class): a symbolic
#'                  description of the model to be fitted.
#' @param data      an optional data frame, list or environment
#'                  (or object coercible by \code{as.data.frame}
#'                  to a data frame) containing the variables in the model.
#' @param scaling   method of scaling to be used. Possible choices
#'                  are: "none", "zscore", "minmax", "medmad" (see
#'                  \code{\link{ilm}} for learning more)
#' @param weighting weights to be applied to all the \emph{variables}
#'                  (notice: this is different than \code{weights}
#'                  parameter in \code{\link{lm}}).
#'
#' @seealso \code{\link{ilm}}
#'
#' @export

plotLinear <- function(formula, data, scaling = "none", weighting = 1)
  UseMethod("plotLinear")


#' @export

plotLinear.formula <- function(formula, data,
                               scaling = c("none", "zscore", "minmax", "medmad"),
                               weighting = 1) {

  scaling <- match.arg(scaling)

  if (missing(data))
    data <- parent.frame()

  mf <- model.frame(formula = formula, data = data)
  y <- model.response(mf)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  intercept <- attr(x, "assign") == 0
  x <- x[, !intercept, drop = FALSE]

  plotLinear.default(x, y, scaling = scaling, weighting = weighting)

}


#' @export

plotLinear.default <- function(x, y,
                               scaling = c("none", "zscore", "minmax", "medmad"),
                               weighting = 1) {

  scaling <- match.arg(scaling)
  x <- as.matrix(x)
  xnew <- variate(x, y, scaling = scaling, weighting = weighting)$variate
  plot(xnew, y, xlab = sprintf("variate\n~ %s", paste0(colnames(x), collapse =" + ")))
  lines(lowess(xnew, y), col = "red")

}

