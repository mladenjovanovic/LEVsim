#' Estimate Reps to Failure Profile using Grid Search
#'
#' @description Finds the \code{L0_reps_drop} and \code{V0_reps_drop} parameters using the grid
#'     search that minimizes the RMSE given the performed \code{load} and \code{MNR} and \code{V0},
#'     \code{L0}, and \code{v1RM} parameters.
#'
#' @param load Numeric vector indicating absolute load used. If relative load is used,
#'     then use \code{relative_load = TRUE}
#' @param MNR Numeric vector indicating maximum number of repetitions performed
#' @param V0 Profile parameter
#' @param L0 Profile parameter
#' @param v1RM Profile parameter
#' @param L0_rep_drop Search vector parameters
#' @param V0_rep_drop Search vector parameters
#' @param relative_load If \code{load} is relative, then absolute load is calculated by
#'     multiplying with profile 1RM
#' @param return_multiple Should multiple solutions be reported? Defaults is \code{FALSE}
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param na.rm Should missing values be removed? Defaults is \code{FALSE}
#' @returns Object \code{LEV_estimate}
#' @export
#' @examples
#' RTF_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   create_summary() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "RTF")
#'
#' m1 <- estimate_RTF_grid(
#'   RTF_df$load,
#'   RTF_df$nRM
#' )
#'
#' m1
estimate_RTF_grid <- function(load,
                              MNR,
                              V0 = 1.8,
                              L0 = 180,
                              v1RM = 0.2,
                              L0_rep_drop = seq(0, 0.15, by = 0.005),
                              V0_rep_drop = seq(0, 0.15, by = 0.005),
                              relative_load = FALSE,
                              return_multiple = FALSE,
                              max_reps = 100,
                              na.rm = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  load_index <- NULL
  solution <- NULL
  true_rep_velocity <- NULL
  failed_rep <- NULL
  pred_MNR <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  `1RM` <- get_load_at_velocity(V0, L0, v1RM)

  if (relative_load == TRUE) load <- load * `1RM`

  # Put data into data frame
  df <- data.frame(
    load = load,
    MNR = MNR,
    V0 = V0,
    L0 = L0,
    v1RM = v1RM
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  parameters <- tidyr::expand_grid(
    V0_rep_drop = V0_rep_drop,
    L0_rep_drop = L0_rep_drop
  ) %>%
    dplyr::filter(!(V0_rep_drop == 0 & L0_rep_drop == 0)) %>%
    dplyr::mutate(solution = seq_along(V0_rep_drop))

  # Perform RTF sets
  df_reps <- df %>%
    dplyr::mutate(load_index = seq_along(load)) %>%
    tidyr::expand_grid(parameters) %>%
    tidyr::expand_grid(rep = seq(1, max_reps)) %>%
    # Do the predictions
    dplyr::mutate(
      get_reps_velocity(
        V0 = V0,
        V0_rep_drop = V0_rep_drop,
        L0 = L0,
        L0_rep_drop = L0_rep_drop,
        biological_variation_additive = 0,
        biological_variation_multiplicative = 0,
        instrumentation_noise_additive = 0,
        instrumentation_noise_multiplicative = 0,
        rep = rep,
        load = load
      )
    ) %>%
    dplyr::group_by(load_index, load, MNR, solution, V0_rep_drop, L0_rep_drop) %>%
    dplyr::mutate(failed_rep = true_rep_velocity <= v1RM) %>%
    dplyr::summarise(pred_MNR = sum(failed_rep == FALSE)) %>%
    dplyr::ungroup()

  # Get model performance
  model_fit <- df_reps %>%
    dplyr::group_by(solution, V0_rep_drop, L0_rep_drop) %>%
    dplyr::reframe(
      data.frame(LEV_model_fit(
        observed = MNR,
        predicted = pred_MNR,
        na.rm = na.rm
      ))
    )

  # Get the minimum RMSE
  best_solutions <- model_fit$solution[model_fit$RMSE == min(model_fit$RMSE)]

  if (return_multiple == FALSE) {
    best_solutions <- best_solutions[[1]]
  }

  L0_rep_drop <- model_fit$L0_rep_drop[model_fit$solution %in% best_solutions]
  V0_rep_drop <- model_fit$V0_rep_drop[model_fit$solution %in% best_solutions]

  model_fit <- model_fit[model_fit$solution %in% best_solutions, ]

  df_reps <- df_reps[df_reps$solution %in% best_solutions, ] %>%
    dplyr::arrange(solution, load_index) %>%
    dplyr::mutate(residuals = pred_MNR - MNR)

  # Return object
  new_profile_estimate(
    parameters = list(
      L0_rep_drop = L0_rep_drop,
      V0_rep_drop = V0_rep_drop
    ),
    model_fit = model_fit,
    model = NULL,
    data = df_reps
  )
}
