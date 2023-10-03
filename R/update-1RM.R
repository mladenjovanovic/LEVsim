#' Update 1RM
#'
#' @description Function that updates 1RM in the LEV profile object.
#' @param LEV_profile \code{LEV_profile} object, returned by \code{\link{create_visits}} function
#' @param what Character string indicating whether \code{visit 1RM} or \code{prescription 1RM} should be updated
#' @param athlete Vector of athletes to be updated. Default is \code{NULL}
#' @param visit Vector of visits to be updated. Default is \code{NULL}
#' @param updated_1RM Vector of 1RM values to be applied. Default is \code{NULL}
#' @export
#' @examples
#' test <- create_athletes(1) %>%
#'   create_visits(1:5) %>%
#'   create_visit_1RM()
#'
#' test_df <- test %>%
#'   as.data.frame()
#'
#' updated_df <- test %>%
#'   update_1RM(what = "visit 1RM", athlete = "Athlete 1", visit = 3, updated_1RM = 200) %>%
#'   as.data.frame()
#'
#' test_df$visit_1RM
#' updated_df$visit_1RM
update_1RM <- function(LEV_profile,
                       what = "visit 1RM",
                       athlete = NULL,
                       visit = NULL,
                       updated_1RM = NULL) {

  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(LEV_profile, stop_running = TRUE)

  # Check the what 1RM to update type
  if ((length(what) != 1) | !(what %in% c("visit 1RM", "prescription 1RM"))) {
    stop("Wrong 1RM type. Please use 'visit 1RM' or 'prescription 1RM'", call. = FALSE)
  }

  # Save the data in data frame
  updated_df <- tidyr::tibble(
    athlete = athlete,
    visit = visit,
    updated_1RM = updated_1RM
  )

  # Cycle through each athlete in the LEV_profile
  res <- purrr::map(LEV_profile, function(profile) {
    # Cycle through visits
    profile$visit <- purrr::map(profile$visit, function(visit) {
      visit_tmp <- visit
      class(visit_tmp) <- "list"
      visit_info <- dplyr::bind_rows(visit_tmp)

      # Check if there are missing columns
      if (!("athlete" %in% colnames(updated_df))) {
        updated_df$athlete <- visit_info$athlete
      }

      if (!("visit" %in% colnames(updated_df))) {
        updated_df$visit <- visit_info$visit
      }

      if (!("updated_1RM" %in% colnames(updated_df))) {
        updated_df$updated_1RM <- NA
      }

      visit_info <- dplyr::left_join(visit_info, updated_df, by = c("athlete", "visit"))

      if (what == "visit 1RM") {
        visit$visit_1RM <- ifelse(is.na(visit_info$updated_1RM), visit$visit_1RM, visit_info$updated_1RM)
      } else if (what == "prescription 1RM") {
        visit$prescription_1RM <- ifelse(is.na(visit_info$updated_1RM), visit$prescription_1RM, visit_info$updated_1RM)
      }

      visit
    })

    profile
  })

  class(res) <- "LEV_profile"
  return(res)
}
