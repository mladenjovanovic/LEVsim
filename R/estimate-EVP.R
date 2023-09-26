#' Estimate Exertion-Velocity Profile
#'
#' @description Finds the \code{L0_reps_drop} and \code{V0_reps_drop} parameters that
#'     minimize the sum of squares given the performed \code{rep}, \code{load}, and \code{velocity}
#'      and \code{V0} and \code{L0} parameters.
#'
#' @param rep Numeric vector indicating repetition
#' @param load Numeric vector indicating absolute load used. If relative load is used,
#'     then use \code{relative_load = TRUE}
#' @param velocity Numeric vector indicating repetition velocity
#' @param V0 Profile parameter
#' @param L0 Profile parameter
#' @param v1RM Profile parameter. Used only if \code{relative_load = TRUE}
#' @param L0_rep_drop_limits Two element search vector, default is \code{c(0, 0.2)}
#' @param V0_rep_drop_limits Two element search vector, default is \code{c(0, 0.2)}
#' @param model_weights Observation weights. Default is \code{1}, implying
#'     equal importance of the observations
#' @param relative_load If \code{load} is relative, then absolute load is calculated by
#'     multiplying with profile 1RM
#' @param na.rm Should missing values be removed? Defaults is \code{FALSE}
#' @param ... forwarded to \link[stats]{optim}
#' @returns Object \code{LEV_estimate}
#' @export
#' @examples
#' RTF_df <- create_profiles(biological_variation_multiplicative = 0.05, L0_rep_drop = 0.06) %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "RTF")
#'
#' m1 <- estimate_EVP(
#'   rep = RTF_df$rep,
#'   load = RTF_df$load,
#'   velocity = RTF_df$measured_rep_velocity)
#'
#' m1
estimate_EVP <- function(rep,
                         load,
                         velocity,
                         V0 = 1.8,
                         L0 = 180,
                         v1RM = 0.2,
                         L0_rep_drop_limits = c(0, 0.2),
                         V0_rep_drop_limits = c(0, 0.2),
                         model_weights = 1,
                         relative_load = FALSE,
                         na.rm = FALSE,
                         ...) {


  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  true_rep_velocity <- NULL
  pred_velocity <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  `1RM` <- get_load_at_velocity(V0, L0, v1RM)

  if(relative_load == TRUE) load <- load * `1RM`

  # Put data into data frame
  df <- data.frame(
    rep = rep,
    load = load,
    velocity = velocity,
    V0 = V0,
    L0 = L0,
    model_weights = model_weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # optimization function
  opt_func <- function(par) {
    # Predict rep velocities
    pred_df <- df %>%
      dplyr::mutate(
        get_reps_velocity(
          V0 = V0,
          V0_rep_drop = par[2],
          L0 = L0,
          L0_rep_drop = par[1],
          biological_variation_additive = 0,
          biological_variation_multiplicative = 0,
          instrumentation_noise_additive = 0,
          instrumentation_noise_multiplicative = 0,
          rep = rep,
          load = load
        )
      )

    # Return sum of squares
    sum((pred_df$velocity - pred_df$true_rep_velocity)^2 * pred_df$model_weights)

  }

  get_optim_params <- function() {
    tryCatch(
      {
        stats::optim(
          par = c(L0_rep_drop_limits[[1]], V0_rep_drop_limits[[1]]),
          fn = opt_func,
          method = "L-BFGS-B",
          lower = c(L0_rep_drop_limits[[1]], V0_rep_drop_limits[[1]]),
          upper = c(L0_rep_drop_limits[[2]], V0_rep_drop_limits[[2]]),
          ...
        )
      },
      error = function(cond) {
        return(list(par = c(NA, NA), value = NA))
      },
      warning = function(cond) {
        return(list(par = c(NA, NA), value = NA))
      }
    )
  }

  results <- get_optim_params()

  # Create results
  parameters <- list(
    L0_rep_drop = results$par[[1]],
    V0_rep_drop = results$par[[2]])

  df <- df %>%
    dplyr::mutate(
      get_reps_velocity(
        V0 = V0,
        V0_rep_drop = parameters$V0_rep_drop,
        L0 = L0,
        L0_rep_drop = parameters$L0_rep_drop,
        biological_variation_additive = 0,
        biological_variation_multiplicative = 0,
        instrumentation_noise_additive = 0,
        instrumentation_noise_multiplicative = 0,
        rep = rep,
        load = load
      )
    ) %>%
    dplyr::select(rep, load, velocity, true_rep_velocity) %>%
    dplyr::rename(pred_velocity = true_rep_velocity) %>%
    dplyr::mutate(residuals = pred_velocity - velocity)

  # Return object
  new_profile_estimate(
    parameters = parameters,
    model_fit = LEV_model_fit(
      observed = df$velocity,
      predicted = df$pred_velocity),
    model = NULL,
    data = df)
}
