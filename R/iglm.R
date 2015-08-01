

#' @rdname ilm
#' @export

iglm <- function(formula, data, family = gaussian,
                 scaling = c("none", "zscore", "minmax", "medmad"),
                 weighting = 1) {

  if (length(all.vars(formula)) == 1)
    stop("Improper formula.")
  scaling <- match.arg(scaling)

  if (missing(data))
    data <- parent.frame()

  if (is.character(family))
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  mf <- model.frame(formula = formula, data = data)
  mt <- attr(mf, "terms")
  y <- model.response(mf)
  x <- model.matrix(attr(mf, "terms"), data = mf)

  est <- iglm.fit(x, y, family = family, scaling = scaling, weighting = weighting)

  est$residuals <- y - est$fitted.values
  est$call <- match.call()
  est$terms <- mt
  class(est) <- c("iglm", "ilm", "glm", "lm")

  est

}


#' @rdname ilm
#' @export

iglm.fit <- function(x, y, family = gaussian(),
                     scaling = "none", weighting = 1) {

  intercept <- attr(x, "assign") == 0
  vars <- colnames(x)

  if (any(intercept))
    x <- x[, !intercept, drop = FALSE]

  pars <- variate(x, y, scaling = scaling, weighting = weighting)
  weighting <- pars$weighting
  variate <- pars$variate

  if (any(intercept)) {
    xnew <- model.matrix(~ 1 + variate, data.frame(variate = variate))
    est <- glm.fit(xnew, y, family = family)
  } else {
    xnew <- model.matrix(~ 0 + variate, data.frame(variate = variate))
    est <- glm.fit(xnew, y, family = family)
  }

  if (length(coef(est)) == 2)
    fit <- coef(est)[1] + coef(est)[2] * variate
  else
    fit <- coef(est)[1] * variate

  names(fit) <- rownames(x)
  est$fitted.values <- est$family$linkinv(fit)
  est$weighting <- weighting
  est$scaling$center <- pars$center
  est$scaling$scale <- pars$scale
  if (any(intercept))
    est$formula <- as.formula(sprintf("~ 1 + %s", paste0(vars, collapse = " + ")))
  else
    est$formula <- as.formula(sprintf("~ 0 + %s", paste0(vars, collapse = " + ")))

  est

}

