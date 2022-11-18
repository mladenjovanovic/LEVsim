#' Create prescription 1RM
#'
#' This function creates prescription 1RM within provided \code{LEV_profile}
#'
#' @param LEV_profile Object returned from \code{\link{create_visits}}
#' @param init_1RM Initial 1RM value
#' @param change_1RM 1RM change per visit
#' @export
#' @examples
#' set.seed(1667)
#' sets <- create_profiles(1, load_increment = 1) %>%
#'   create_visits(1:10) %>%
#'   create_prescription_1RM(100, 2.5) %>%
#'   create_sets(load = 0.8, load_type = "prescription 1RM")
#'
#' plot(sets, x_var = "visit", y_var = "load", reps = 1, facet = NULL)
#'
#' plot(sets, x_var = "visit", y_var = "prescription_1RM", facet = NULL)
create_prescription_1RM <- function(LEV_profile, init_1RM, change_1RM) {

  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(LEV_profile, stop_running = TRUE)

  # Cycle through each athlete in the LEV_profile
  res <- purrr::map(LEV_profile, function(profile) {
    # Cycle through visits
    profile$visit <- purrr::map(profile$visit, function(visit) {
      visit$prescription_1RM <- init_1RM + (visit$visit - 1) * change_1RM
      visit
    })

    profile
  })

  class(res) <- "LEV_profile"
  return(res)
}
