#' Estimate Reps to Failure Profile using Optim and Fractional MNR
#'
#' @description Finds the \code{L0_reps_drop} and \code{V0_reps_drop} parameters using the \link[stats]{optim}
#'     function that minimizes the sum-of-squares-error given the performed \code{load} and \code{MNR} and \code{V0},
#'     \code{L0}, and \code{v1RM} parameters. Fractional MNRs are utilized in this function compare to
#'     the \code{\link{estimate_RTF_grid}}
#'
#' @param load Numeric vector indicating absolute load used. If relative load is used,
#'     then use \code{relative_load = TRUE}
#' @param MNR Numeric vector indicating maximum number of repetitions performed
#' @param V0 Profile parameter
#' @param L0 Profile parameter
#' @param v1RM Profile parameter
#' @param L0_rep_drop_limits Two element search vector, default is \code{c(0, 0.2)}
#' @param V0_rep_drop_limits Two element search vector, default is \code{c(0, 0.2)}
#' @param model_weights Observation weights. Default is \code{1}, implying
#'     equal importance of the observations
#' @param relative_load If \code{load} is relative, then absolute load is calculated by
#'     multiplying with profile 1RM
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param na.rm Should missing values be removed? Defaults is \code{FALSE}
#' @param ... forwarded to \link[stats]{optim}
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
#' m1 <- estimate_RTF(
#'   RTF_df$load,
#'   RTF_df$nRM
#' )
#'
#' m1
estimate_RTF <- function(load,
                         MNR,
                         V0 = 1.8,
                         L0 = 180,
                         v1RM = 0.2,
                         L0_rep_drop_limits = c(0, 0.2),
                         V0_rep_drop_limits = c(0, 0.2),
                         model_weights = 1,
                         relative_load = FALSE,
                         max_reps = 100,
                         na.rm = FALSE,
                         ...) {


  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
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
    v1RM = v1RM,
    model_weights = model_weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # optimization function
  opt_func <- function(par) {
    cat(par, "\n")
    # browser()
    # Predict rep velocities
    pred_df <- df %>%
      dplyr::mutate(
        pred_MNR = get_MNR(
          load = load,
          L0 = L0,
          V0 = V0,
          v1RM = v1RM,
          L0_rep_drop = par[[1]],
          V0_rep_drop = par[[2]],
          fractional = TRUE,
          max_reps = max_reps,
          na.rm = na.rm
        )
      )

    # Return sum of squares
    err <- sum((pred_df$pred_MNR - pred_df$MNR)^2 * pred_df$model_weights)

    if (is.na(err)) {
      sum((max_reps - pred_df$MNR)^2 * pred_df$model_weights)
    } else {
      err
    }
  }

  get_optim_params <- function() {
    tryCatch(
      {
        stats::optim(
          par = c(
            0.5 * (L0_rep_drop_limits[[1]] + L0_rep_drop_limits[[2]]),
            0.5 * (V0_rep_drop_limits[[1]] + V0_rep_drop_limits[[2]])
          ),
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
    V0_rep_drop = results$par[[2]]
  )

  df <- df %>%
    dplyr::mutate(
      pred_MNR = get_MNR(
        load = load,
        L0 = L0,
        V0 = V0,
        v1RM = v1RM,
        L0_rep_drop = parameters$L0_rep_drop,
        V0_rep_drop = parameters$V0_rep_drop,
        fractional = TRUE,
        max_reps = max_reps,
        na.rm = na.rm
      )
    ) %>%
    dplyr::select(load, MNR, pred_MNR) %>%
    dplyr::mutate(residuals = pred_MNR - MNR)

  # Return object
  new_profile_estimate(
    parameters = parameters,
    model_fit = LEV_model_fit(
      observed = df$MNR,
      predicted = df$pred_MNR
    ),
    model = NULL,
    data = df
  )
}
