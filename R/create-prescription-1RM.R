#' Create prescription 1RM
#'
#' This function creates prescription 1RM within provided \code{LEV_profile}
#'
#' @param LEV_profile Object returned from \code{\link{create_visits}}
#' @param init_1RM Initial 1RM value. If \code{NULL} (default), then initial 1RM is estimated using
#'      profile parameters and used
#' @param change_1RM 1RM change per visit. Default is 0
#' @param load_perc Percentage to be used to calculate loads using theoretical visit profile 1RM. This
#'      is utilized when \code{init_RM} equals \code{NULL}
#' @param buffer Default is 1, but can be, for example 0.8 of the estimated 1RM to be used for
#'      prescription
#' @export
#' @examples
#' set.seed(1667)
#' sets <- create_profiles(1, load_increment = 1) %>%
#'   create_visits(1:10) %>%
#'   create_prescription_1RM() %>%
#'   create_sets(load = c(0.7, 0.8), load_type = "prescription 1RM")
#'
#' plot(sets, x_var = "visit", y_var = "load", reps = 1, facet = NULL)
#' plot(sets, x_var = "visit", y_var = "prescription_1RM", facet = NULL)
#'
#'
#' set.seed(1667)
#' sets <- create_profiles(1, load_increment = 1) %>%
#'   create_visits(1:10) %>%
#'   create_prescription_1RM(120, 2.5) %>%
#'   create_sets(load = c(0.7, 0.8), load_type = "prescription 1RM")
#'
#' plot(sets, x_var = "visit", y_var = "load", reps = 1, facet = NULL)
#' plot(sets, x_var = "visit", y_var = "prescription_1RM", facet = NULL)
create_prescription_1RM <- function(LEV_profile,
                            init_1RM = NULL,
                            change_1RM = 0,
                            load_perc = seq(0.6, 1.2, by = 0.025),
                            buffer = 1) {

  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(LEV_profile, stop_running = TRUE)

  # Cycle through each athlete in the LEV_profile
  res <- purrr::map(LEV_profile, function(profile) {
    # Cycle through visits

    # If init_1RM is not provided, test it
    if (is.null(init_1RM)) {
      load <- get_load_rounded(profile$profile$`1RM` * load_perc, profile$profile$load_increment)
      oneRM_trials <- get_sets(profile$profile, load, max_reps = 1)

      last_try <- min(which(oneRM_trials$failed_rep)) - 1
      init_1RM <- load[[last_try]] * buffer
    }

    profile$visit <- purrr::map(profile$visit, function(visit) {
      visit$prescription_1RM <- init_1RM + (visit$visit - 1) * change_1RM
      visit
    })

    profile
  })

  class(res) <- "LEV_profile"
  return(res)
}
