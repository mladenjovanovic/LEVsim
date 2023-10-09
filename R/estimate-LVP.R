#' Estimate Load-Velocity Profile
#' @description Finds the \code{L0} and \code{V0} parameters using simple linear
#'     regression given the \code{load} and \code{velocity} performance
#'
#' @param load Numeric vector indicating load used
#' @param velocity Numeric vector indicating repetition velocity
#' @param model_weights Observation weights. Default is \code{load}, implying
#'     the higher the load, the higher the importance of the observation
#' @param na.rm Should missing values be removed? Defaults is \code{FALSE}
#' @returns Object \code{LEV_estimate}
#' @export
#' @examples
#' LV_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "LV")
#'
#' m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)
#'
#' m1
estimate_LVP <- function(load,
                         velocity,
                         model_weights = load,
                         na.rm = FALSE) {



  # Put data into data frame
  df <- data.frame(
    load = load,
    velocity = velocity,
    model_weights = model_weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Create absolute LVP
  m1 <- stats::lm(velocity ~ load, df, weights = model_weights)

  df$pred_velocity <- stats::predict(m1, newdata = data.frame(load = df$load))
  df$residuals <- df$pred_velocity - df$velocity

  # Parameters
  V0 <- stats::coef(m1)[[1]]
  L0 <- -stats::coef(m1)[[1]] / stats::coef(m1)[[2]]

  # Model fit
  model_fit <- LEV_model_fit(
    model = m1,
    observed = df$velocity,
    predicted = df$pred_velocity,
    na.rm = na.rm
  )

  # Return object
  new_profile_estimate(
    parameters = list(V0 = V0, L0 = L0),
    model_fit = model_fit,
    model = m1,
    data = df
  )
}
