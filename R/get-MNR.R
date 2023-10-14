# Function to find MNR (nRM)
find_MNR <- function(rep, failed, velocity, v1RM, fractional) {
  if (!any(failed == TRUE)) {
    return(NA)
  }

  if (!any(failed == FALSE)) {
    return(0)
  }

  first_failed_rep <- get_last_rep(rep, failed)
  last_succesful_rep <- first_failed_rep - 1

  if (fractional == FALSE) {
    last_succesful_rep
  } else {
    first_failed_rep_velocity <- velocity[[first_failed_rep]]
    last_succesful_rep_velocity <- velocity[[last_succesful_rep]]

    last_succesful_rep + (last_succesful_rep_velocity - v1RM) / (last_succesful_rep_velocity - first_failed_rep_velocity)
  }
}

#' Get Maximum Number of Reps
#'
#' @description Find the maximum number of repetitions (\code{MNR}, or \code{nRM}) given the
#'     \code{load}, \code{L0}, \code{V0}, \code{v1RM}, \code{L0_rep_drop}, and \code{V0_rep_drop}
#'     parameters. This functions uses true repetition velocity to discern MNR
#'
#' @param load Numeric vector indicating absolute load used. If relative load is used,
#'     then use \code{relative_load = TRUE}
#' @param V0 Profile parameter
#' @param L0 Profile parameter
#' @param v1RM Profile parameter
#' @param L0_rep_drop Profile parameter
#' @param V0_rep_drop Profile parameter
#' @param relative_load If \code{load} is relative, then absolute load is calculated by
#'     multiplying with profile 1RM
#' @param fractional Should fractional MNR be returned? Default is \code{FALSE}
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param na.rm Should missing values be removed? Defaults is \code{FALSE}
#' @returns Numeric vector
#' @export
#' @examples
#' get_MNR(c(140, 145, 150), fractional = FALSE)
#' get_MNR(c(140, 145, 150), fractional = TRUE)
#' get_MNR(150, L0 = c(170, 180, 190), fractional = TRUE)
get_MNR <- function(load,
                    V0 = 1.8,
                    L0 = 180,
                    v1RM = 0.2,
                    L0_rep_drop = 0.045,
                    V0_rep_drop = 0,
                    relative_load = FALSE,
                    fractional = FALSE,
                    max_reps = 100,
                    na.rm = FALSE) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  set_index <- NULL
  true_rep_velocity <- NULL
  failed_rep <- NULL
  pred_MNR <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++


  `1RM` <- get_load_at_velocity(V0, L0, v1RM)

  if (relative_load == TRUE) load <- load * `1RM`

  # Put data into data frame
  df <- data.frame(
    load = load,
    V0 = V0,
    L0 = L0,
    v1RM = v1RM,
    L0_rep_drop = L0_rep_drop,
    V0_rep_drop = V0_rep_drop
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Perform RTF sets
  df_reps <- df %>%
    dplyr::mutate(set_index = seq_along(load)) %>%
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
    dplyr::group_by(set_index) %>%
    dplyr::mutate(failed_rep = true_rep_velocity <= v1RM) %>%
    dplyr::summarise(
      MNR = find_MNR(
        rep = rep,
        failed = failed_rep,
        velocity = true_rep_velocity,
        v1RM = v1RM[[1]],
        fractional = fractional
      )
    ) %>%
    dplyr::ungroup()
  df_reps$MNR
}
