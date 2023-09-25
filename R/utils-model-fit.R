# Function to return model fit metrics
LEV_model_fit <- function(model = NULL, observed, predicted, na.rm = FALSE) {
  resid <- observed - predicted

  if (is.null(model)) {
    RSE <- NA
  } else {
    RSE <- summary(model)$sigma
  }

  list(
    RSE = RSE,
    bias = mean(resid, na.rm = na.rm),
    minErr = min(resid, na.rm = na.rm),
    maxErr = max(resid, na.rm = na.rm),
    maxAbsErr = max(abs(resid), na.rm = na.rm),
    RMSE = sqrt(mean(resid^2, na.rm = na.rm)),
    MAE = mean(abs(resid), na.rm = na.rm),
    MAPE = 100 * mean(abs(resid / observed), na.rm = na.rm)
  )
}
