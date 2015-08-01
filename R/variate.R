

variate <- function(x, y, scaling = "none", weighting = 1) {

  x <- as.matrix(x)

  if (scaling == "zscore") {
    x <- scale(x, center = TRUE, scale = TRUE)
  } else if (scaling == "minmax") {
    mins <- apply(x, 2, min)
    maxs <- apply(x, 2, max)
    x <- scale(x, center = mins, scale = maxs-mins)
  } else if (scaling == "medmad") {
    meds <- apply(x, 2, median)
    mads <- apply(x, 2, mad)
    x <- scale(x, center = meds, scale = mads)
  }

  if (is.function(weighting)) {
    weighting <- apply(x, 2, weighting)
  } else if (weighting[1] == "cor") {
    weighting <- apply(x, 2, cor, y)
  } else if (weighting[1] == "sign") {
    weighting <- apply(x, 2, function(v) sign(cor(v, y)))
  } else if (weighting[1] == "lmsign") {
    xmm <- model.matrix(~ ., as.data.frame(x))
    weighting <- sign(coef(lm.fit(xmm, y))[-1])
  }

  if (length(weighting) == 1)
    weighting <- rep(weighting, ncol(x))

  names(weighting) <- colnames(x)
  variate <- as.vector(x %*% weighting)

  list(variate = variate,
       weighting = weighting,
       center = attr(x, "scaled:center"),
       scale = attr(x, "scaled:scale"))

}
