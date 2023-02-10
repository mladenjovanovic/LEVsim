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


#' S3 method for printing LEV Sets Summaries
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

#' S3 method for extracting LEV Sets Summaries
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


#' S3 method for extracting LEV Sets Summaries
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


#' S3 method for plotting LEV Sets Summaries
#'
#' @details S3 method for plotting LEV Sets Summaries provides plenty of
#'     flexibility and control by using the \code{...} parameters.
#'     User can filter variables, use different x and y scales,
#'     provides colors and facets. Here are the extra parameters your
#'     can utilize:
#'     \describe{
#'      \item{athletes}{Althetes to keep in}
#'      \item{visits}{Visits to keep in}
#'      \item{sets}{Sets to keep in}
#'      \item{loads}{Loads to keep in}
#'      \item{RTFs}{Reps-To-Failure indicator (TRUE/FALSE) to keep in}
#'      \item{reps}{Repetitions to keep in}
#'      \item{RIRs}{Reps-In-Reserve to keep in}
#'      \item{y_var}{Variable on y-axis. Default is 'measured_rep_velocity'}
#'      \item{x_var}{Variable on x-axis. Default is 'load'}
#'      \item{color}{What variable should be used to coloring groups? Default is none}
#'      \item{facet}{Variable to be used for plot faceting. Athletes are already faceted,
#'          and this parameter allows for adding extra level. Default is 'visit'}
#'     }
#'
#' Please check the examples section for few examples
#'
#' @param x Object of class \code{LEV_sets}
#' @param type Type of plot. Default is "athletes" which creates facets
#'     for each athlete. Another option is "pooled", which doesn't create
#'     facets for athletes by default. One can use \code{facet} parameter
#'     to create facets using another variable. See Details
#' @param ... Extra arguments. See Details
#' @export
#' @examples
#' set.seed(1667)
#' sets <- create_athletes(2) %>%
#'   create_visits(1) %>%
#'   create_visit_1RM() %>%
#'   create_sets(load = c(0.7, 0.8, 0.9), load_type = "visit 1RM") %>%
#'   create_summary()
#'
#' sets
#' plot(sets)
plot.LEV_summary <- function(x, type = "athletes", ...) {
  if (!(type %in% c("athletes", "pooled"))) {
    stop("Plot type can be either 'athletes' or 'pooled'", call. = FALSE)
  }

  plot_data <- stats::coef(x)

  plot_LEV_summary(plot_data, type = type, ...)
}

# ===================================
plot_LEV_summary <- function(df,
                             type = "athletes",
                             athletes = NULL,
                             visits = NULL,
                             sets = NULL,
                             loads = NULL,
                             RTFs = NULL,
                             reps = NULL,
                             RIRs = NULL,
                             color = NULL,
                             facet = "visit") {


  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  athlete <- NULL
  visit <- NULL
  set <- NULL
  load_index <- NULL
  RTF <- NULL
  `%1RM` <- NULL
  `RIR` <- NULL
  best_measured_rep_velocity <- NULL
  first_measured_rep_velocity <- NULL
  last_measured_rep_velocity <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # If null, use visit so it doesn't throw an error
  if (is.null(facet)) {
    facet.tmp <- "visit"
    facet_name <- as.name(facet.tmp)
  } else {
    facet_name <- as.name(facet)
  }

  df <- df %>%
    dplyr::mutate(
      facet_var = !!facet_name,
      set = factor(set)
    )

  if (!is.null(athletes)) {
    df <- df %>%
      dplyr::filter(athlete %in% athletes)
  }

  if (!is.null(visits)) {
    df <- df %>%
      dplyr::filter(visit %in% visits)
  }

  if (!is.null(sets)) {
    df <- df %>%
      dplyr::filter(set %in% sets)
  }

  if (!is.null(loads)) {
    df <- df %>%
      dplyr::filter(load %in% loads)
  }

  if (!is.null(RTFs)) {
    df <- df %>%
      dplyr::filter(RTF %in% RTFs)
  }

  if (!is.null(reps)) {
    df <- df %>%
      dplyr::filter(rep %in% reps)
  }

  if (!is.null(RIRs)) {
    df <- df %>%
      dplyr::filter(RIR %in% RIRs)
  }

  if (is.null(color)) {
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = load_index, xend = load_index, y = best_measured_rep_velocity, yend = last_measured_rep_velocity))
  } else {
    gg <- ggplot2::ggplot(df, ggplot2::aes_string(x = "load_index", xend = "load_index", y = "best_measured_rep_velocity", yend = "last_measured_rep_velocity", color = color))
  }

  gg <- gg +
    ggplot2::theme_linedraw() +
    ggplot2::geom_segment(alpha = 0.9, size = 1, arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"))) +
    ggplot2::geom_text(ggplot2::aes(y = best_measured_rep_velocity + 0.025, label = load), vjust = "bottom", size = 1.5, alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(y = last_measured_rep_velocity - 0.025, label = paste0(RIR, "RIR")), vjust = "top", size = 1.5, alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(y = last_measured_rep_velocity + 0.5 * (best_measured_rep_velocity - last_measured_rep_velocity), label = reps), vjust = -1, size = 1.5, alpha = 0.8, angle = 90)
    #ggplot2::geom_label(ggplot2::aes(y = best_measured_rep_velocity + 0.065, label = paste0(reps, " w/", load, "\n@", RIR, "RIR")), vjust = "center", size = 2)

  if (type == "athletes") {
    if (!is.null(facet)) {
      gg <- gg +
        ggplot2::facet_wrap(~ athlete + facet_var)
    } else {
      gg <- gg +
        ggplot2::facet_wrap(~athlete)
    }
  } else {
    # Do not use athletes as facets
    if (!is.null(facet)) {
      gg <- gg +
        ggplot2::facet_wrap(~facet_var)
    }
  }

  gg <- gg +
    ggplot2::xlab("Set") +
    ggplot2::ylab("Measured Rep Velocity")

  gg
}
