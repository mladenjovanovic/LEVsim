#' S3 method for adding two or more LEV Summaries
#'
#' @param lhs Object of class \code{LEV_sets}
#' @param rhs Object of class \code{LEV_sets}
#'
#' @return Object of class \code{LEV_sets}
#' @export
#'
#' @examples
#' sets1 <- create_athletes(5) %>%
#'   create_visits(1:3) %>%
#'   create_sets(
#'     load = c(70, 80, 90)
#'   ) %>%
#'   create_summary()
#'
#' sets2 <- create_profiles(athletes = c("Mladen", "Milan", "Mihailo")) %>%
#'   create_visits(1:3) %>%
#'   create_sets(
#'     load = c(70, 80, 90)
#'   ) %>%
#'   create_summary()
#'
#' add_sets <- sets1 + sets2
#'
#' add_sets
`+.LEV_summary` <- function(lhs, rhs) {
  # Check of the object is proper LEV_summary
  is_LEV <- validate_LEV_summary(lhs, stop_running = TRUE)
  is_LEV <- validate_LEV_summary(rhs, stop_running = TRUE)

  lhs <- as.data.frame(lhs)
  rhs <- as.data.frame(rhs)

  add_sets <- rbind(lhs, rhs)

  class(add_sets) <- "LEV_summary"

  return(add_sets)
}


#' S3 method for printing LEV Profiles Summaries
#' @param x Object of class \code{LEV_summary}
#' @param ... Extra arguments
#' @export
#' @examples
#' # Create random athletes/profiles
#' sets <- create_athletes(1) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(70, 80, 90)) %>%
#'   create_summary()
#'
#' print(sets)
print.LEV_summary <- function(x, ...) {
  print(stats::coef(x, ...))
}

#' S3 method for extracting LEV Profiles Summaries
#' @param object Object of class \code{LEV_summary}
#' @param ... Extra arguments
#' @export
#' @return Data Frame
#' @examples
#' # Create random athletes/profiles
#' sets <- create_athletes(1) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(70, 80, 90)) %>%
#'   create_summary()
#'
#' sets_params <- coef(sets)
coef.LEV_summary <- function(object, ...) {
  class(object) <- "list"

  sets <- dplyr::bind_rows(object)

  return(sets)
}


#' S3 method for extracting LEV Profiles Summaries
#' @param x Object of class \code{LEV_summary}
#' @param row.names Not used
#' @param optional Not used
#' @param ... Extra arguments
#' @export
#' @return Data Frame
#' @examples
#' # Create random athletes/profiles
#' sets <- create_athletes(1) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(70, 80, 90)) %>%
#'   create_summary()
#'
#' sets_params <- as.data.frame(sets)
as.data.frame.LEV_summary <- function(x, row.names = NULL, optional = FALSE, ...) {
  stats::coef(x, ...)
}
