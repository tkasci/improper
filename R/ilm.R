

#' Improper (generalized) linear model
#'
#' @description \code{ilm} is used for fitting improper linear models
#'              (unit-weighted regression, equal-weights models, model
#'              with correlation weights, Z-score method, random linear
#'              models). Univariate model reduces to univariate linear
#'              regression. \code{iglm} extends \code{ilm} functionality
#'              to generalized linear models.
#'
#' @param formula   an object of class \code{"formula"} (or one that can
#'                  be coerced to that class): a symbolic
#'                  description of the model to be fitted.
#' @param data      an optional data frame, list or environment
#'                  (or object coercible by \code{as.data.frame}
#'                  to a data frame) containing the variables in the model.
#'                  If not found in data, the variables are taken
#'                  from \code{environment(formula)}, typically the environment
#'                  from which \code{ilm} or \code{iglm} is called.
#' @param scaling   method of scaling to be used. Possible choices
#'                  are: "none" for no scaling; "zscore" for
#'                  transforming into Z-scores; "minmax" for
#'                  scaling into [0, 1] range; and "medmad" for
#'                  subtracting median and dividing by MAD. See:
#'                  \code{\link{scale}}.
#' @param weighting weights to be applied to all the \emph{variables}
#'                  (notice: this is different than \code{weights}
#'                  parameter in \code{\link{lm}} etc.). Possible values
#'                  are: single scalar (1 by default);
#'                  a vector of the same length as number of variables
#'                  in the left hand side of \code{formula} (e.g.
#'                  \code{runif(5)} for five weights in random linear
#'                  model with five predictors);
#'                  "cor" for multiplying every variable by its
#'                  correlation with variable on the left hand side
#'                  of \code{formula};
#'                  "sign" for unit weights such that positively
#'                  correlated variables get weight 1 and negatively -1;
#'                  "lmsign" for unit weights similar to "sign" but created
#'                  by taking signs of coefficients from (generalized)
#'                  linear model;
#'                  or a function (applied column-wise) to each of the
#'                  variables in the left hand side of \code{formula}
#'                  (e.g. \code{function(x) 1/sd(x)}).
#' @param family    a description of the error distribution and link function
#'                  to be used in the model. For glm this can be a character
#'                  string naming a family function, a family function or the
#'                  result of a call to a family function. For glm.fit only the
#'                  third option is supported. (See \code{\link{family}} for
#'                  details of family functions.)
#'
#' @examples
#' fit1 <- ilm(cyl ~ mpg + drat + am, data = mtcars)
#'
#' print(fit1)
#' summary(fit1)
#'
#' fit2 <- iglm(am ~ ., data = mtcars, family = binomial)
#'
#' print(fit2)
#' summary(fit2)
#'
#' @references
#' Dawes, Robyn M. (1979). The robust beauty of improper linear
#' models in decision making. American Psychologist, 34, 571-582.
#' @references
#' Graefe, A. (2015). Improving forecasts using equally weighted
#' predictors. Journal of Business Research, 68(8), 1792-1799.
#' @references
#' Wainer, Howard (1976). Estimating coefficients in linear models:
#' It don't make no nevermind. Psychological Bulletin 83(2), 213.
#' @references
#' Dana, J. and Dawes, R.M. (2004). The Superiority of Simple
#' Alternatives to Regression for Social Science Predictions.
#' Journal of Educational and Behavioral Statistics, 29(3), 317-331.
#'
#' @seealso \code{\link{lm}}, \code{\link{lm.fit}},
#'          \code{\link{glm}}, \code{\link{glm.fit}},
#'          \code{\link{scale}}
#'
#' @rdname ilm
#' @export

ilm <- function(formula, data,
                scaling = c("none", "zscore", "minmax", "medmad"),
                weighting = 1) {

  if (length(all.vars(formula)) == 1)
    stop("Improper formula.")
  scaling <- match.arg(scaling)

  if (missing(data))
    data <- parent.frame()

  mf <- model.frame(formula = formula, data = data)
  mt <- attr(mf, "terms")
  y <- model.response(mf)
  x <- model.matrix(attr(mf, "terms"), data = mf)

  est <- ilm.fit(x, y, scaling = scaling, weighting = weighting)

  est$residuals <- y - est$fitted.values
  est$call <- match.call()
  est$terms <- mt
  class(est) <- c("ilm", "lm")

  est

}


#' @rdname ilm
#' @export

ilm.fit <- function(x, y, scaling = "none", weighting = 1) {

  intercept <- attr(x, "assign") == 0
  vars <- colnames(x)

  if (any(intercept))
    x <- x[, !intercept, drop = FALSE]

  pars <- variate(x, y, scaling = scaling, weighting = weighting)
  weighting <- pars$weighting
  variate <- pars$variate

  if (any(intercept)) {
    xnew <- model.matrix(~ 1 + variate, data.frame(variate = variate))
    est <- lm.fit(xnew, y)
  } else {
    xnew <- model.matrix(~ 0 + variate, data.frame(variate = variate))
    est <- lm.fit(xnew, y)
  }

  if (length(coef(est)) == 2)
    fit <- coef(est)[1] + coef(est)[2] * variate
  else
    fit <- coef(est)[1] * variate

  names(fit) <- rownames(x)
  est$fitted.values <- fit
  est$weighting <- weighting
  est$scaling$center <- pars$center
  est$scaling$scale <- pars$scale
  if (any(intercept))
    est$formula <- as.formula(sprintf("~ 1 + %s", paste0(vars, collapse = " + ")))
  else
    est$formula <- as.formula(sprintf("~ 0 + %s", paste0(vars, collapse = " + ")))

  est

}

