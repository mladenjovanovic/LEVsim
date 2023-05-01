#' S3 method for adding two or more LEV Profiles and Visits
#'
#' @param lhs Object of class \code{LEV_profile}
#' @param rhs Object of class \code{LEV_profile}
#'
#' @return Object of class \code{LEV_profile}
#' @export
#'
#' @examples
#' # Add profiles
#' profile1 <- create_athletes(5)
#' profile2 <- create_profiles(athlete = c("Mladen", "Milan", "Mihailo"))
#'
#' added_profiles <- profile1 + profile2
#' added_profiles
#'
#' # Add visits
#' visits1 <- profile1 %>%
#'   create_visits(1:3)
#'
#' visits2 <- profile2 %>%
#'   create_visits(1:10)
#'
#' added_visits <- visits1 + visits2
#' added_visits
`+.LEV_profile` <- function(lhs, rhs) {

  # Check of the object is proper LEV_profile
  is_LEV <- validate_LEV_profile(lhs, stop_running = TRUE)
  is_LEV <- validate_LEV_profile(rhs, stop_running = TRUE)

  class(lhs) <- "list"
  class(rhs) <- "list"
  add_profiles <- c(lhs, rhs)

  class(add_profiles) <- "LEV_profile"

  return(add_profiles)
}

#' S3 method for printing LEV Profiles and Visits
#' @param x Object of class \code{LEV_profile}
#' @param ... Extra arguments
#' @export
#' @examples
#' profiles <- create_athletes(10)
#' print(profiles)
#'
#' visits <- profiles %>% create_visits(1:3)
#' print(visits)
print.LEV_profile <- function(x, ...) {
  print(stats::coef(x, ...))
}

#' S3 method for extracting LEV Profiles and Visits parameters
#' @param object Object of class \code{LEV_profile}
#' @param ... Extra arguments
#' @return Data Frame
#' @export
#' @examples
#' profiles <- create_athletes(10)
#' profile_params <- coef(profiles)
#'
#' visits <- profiles %>% create_visits(1:3)
#' visit_params <- coef(visits)
coef.LEV_profile <- function(object, ...) {
  df <- purrr::map_df(object, function(athlete) {
    # In the case when there are no visits
    if (length(athlete$visit) == 0) {
      # Return profiles
      dplyr::bind_cols(athlete$profile)
    } else {
      # Return visits
      df <- purrr::map_df(athlete$visit, function(visit) {
        class(visit) <- "list"
        dplyr::bind_cols(visit)
      })
    }
  })
  df
}

#' S3 method for extracting LEV Profiles and Visits parameters
#' @param x Object of class \code{LEV_profile}
#' @param row.names Not used
#' @param optional Not used
#' @param ... Extra arguments
#' @return Data Frame
#' @export
#' @examples
#' profiles <- create_athletes(10)
#' profile_params <- coef(profiles)
#'
#' visits <- profiles %>% create_visits(1:3)
#' visit_params <- as.data.frame(visits)
as.data.frame.LEV_profile <- function(x, row.names = NULL, optional = FALSE, ...) {
  stats::coef(x, ...)
}


#' S3 method for plotting LEV Profiles
#' @param x Object of class \code{LEV_profile}
#' @param type Type of plot. Default is "athletes" which creates facets
#'     for each athlete. Another option is "pooled", which doesn't create
#'     facets
#' @param ... Extra arguments
#' @return \code{\link[ggplot2]{ggplot}} object
#' @export
#' @examples
#' profiles <- create_athletes(10)
#' plot(profiles)
#'
#' visits <- profiles %>% create_visits(1:3)
#' plot(visits)
plot.LEV_profile <- function(x, type = "athletes", ...) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  L0 <- NULL
  V0 <- NULL
  visit <- NULL
  `1RM` <- NULL
  `v1RM` <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  if (!(type %in% c("athletes", "pooled"))) {
    stop("Plot type can be either 'athletes' or 'pooled'", call. = FALSE)
  }

  plot_data <- as.data.frame(x)

  is_visit <- "visit" %in% colnames(plot_data)

  if (is_visit) {
    plot_data$visit <- factor(plot_data$visit)
  }

  gg <- ggplot2::ggplot(plot_data) +
    ggplot2::theme_linedraw()

  if (is_visit) {
    gg <- gg +
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = L0, y = V0, yend = 0, color = visit),
        alpha = 0.7
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = `1RM`, y = v1RM, yend = v1RM, color = visit),
        alpha = 0.5,
        linetype = "dotted"
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = `1RM`, xend = `1RM`, y = 0, yend = v1RM, color = visit),
        alpha = 0.5,
        linetype = "dotted"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = `1RM`, y = v1RM, color = visit),
        size = 2,
        shape = 21,
        fill = "white"
      )
  } else {
    gg <- gg +
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = L0, y = V0, yend = 0),
        color = "#5DA5DA"
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = `1RM`, y = v1RM, yend = v1RM),
        alpha = 0.5,
        linetype = "dotted"
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = `1RM`, xend = `1RM`, y = 0, yend = v1RM),
        alpha = 0.5,
        linetype = "dotted"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = `1RM`, y = v1RM),
        size = 2,
        shape = 21,
        fill = "white"
      )
  }

  if (type == "athletes") {
    gg <- gg +
      ggplot2::facet_wrap(~athlete)
  }

  gg <- gg +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::xlab("Load") +
    ggplot2::ylab("Velocity")

  gg
}
