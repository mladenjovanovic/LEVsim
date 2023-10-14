#' Get Maximum Percent of 1RM
#'
#' @description Find the maximum percent of 1RM given the
#'     \code{MNR}, \code{L0}, \code{V0}, \code{v1RM}, \code{L0_rep_drop}, and \code{V0_rep_drop}
#'     parameters.
#'
#' @param MNR Maximum number of repetitions
#' @param V0 Profile parameter
#' @param L0 Profile parameter
#' @param v1RM Profile parameter
#' @param L0_rep_drop Profile parameter
#' @param V0_rep_drop Profile parameter
#' @param max_reps How many maximum reps to generate to search for failure? Default is 100
#' @param interval Two element search vector. Default is \code{c(0.5, 1.1)}
#' @param na.rm Should missing values be removed? Defaults is \code{FALSE}
#' @param ... Forwarded to \code{\link[stats]{uniroot}} function
#' @returns Numeric vector
#' @export
#' @examples
#' get_perc_1RM(c(1, 2, 3, 4, 5))
get_perc_1RM <- function(MNR,
                         V0 = 1.8,
                         L0 = 180,
                         v1RM = 0.2,
                         L0_rep_drop = 0.045,
                         V0_rep_drop = 0,
                         max_reps = 100,
                         interval = c(0.5, 1.1),
                         na.rm = FALSE,
                         ...) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  set_index <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  `1RM` <- get_load_at_velocity(V0, L0, v1RM)

  # Put data into data frame
  df <- data.frame(
    MNR = MNR,
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

  find_perc_1RM <- function(MNR, V0, L0, v1RM, L0_rep_drop, V0_rep_drop, interval, max_reps, na.rm) {
    res <- stats::uniroot(function(x, MNR, V0, L0, v1RM, L0_rep_drop, V0_rep_drop, max_reps, na.rm) {
      pred_MNR <- get_MNR(
        load = x,
        V0 = V0,
        L0 = L0,
        v1RM = v1RM,
        L0_rep_drop = L0_rep_drop,
        V0_rep_drop = V0_rep_drop,
        fractional = TRUE,
        relative_load = TRUE,
        max_reps = max_reps,
        na.rm = na.rm
      )
      pred_MNR <- ifelse(is.na(pred_MNR), 0, pred_MNR)

      MNR - pred_MNR
    },
    interval = interval,
    MNR = MNR,
    V0 = V0,
    L0 = L0,
    v1RM = v1RM,
    L0_rep_drop = L0_rep_drop,
    V0_rep_drop = V0_rep_drop,
    max_reps = max_reps,
    na.rm = na.rm,
    ...
    )

    res$root
  }

  df_perc_1RM <- df %>%
    dplyr::mutate(set_index = seq_along(MNR)) %>%
    dplyr::group_by(set_index) %>%
    dplyr::summarise(perc_1RM = find_perc_1RM(
      MNR = MNR,
      V0 = V0,
      L0 = L0,
      v1RM = v1RM,
      L0_rep_drop = L0_rep_drop,
      V0_rep_drop = V0_rep_drop,
      max_reps = max_reps,
      na.rm = na.rm,
      interval = interval,
      ...
    )) %>%
    dplyr::ungroup()

  df_perc_1RM$perc_1RM
}
