#' S3 method for adding two or more LEV Sets
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
#'   )
#'
#' sets2 <- create_profiles(athletes = c("Mladen", "Milan", "Mihailo")) %>%
#'   create_visits(1:3) %>%
#'   create_sets(
#'     load = c(70, 80, 90)
#'   )
#'
#' add_sets <- sets1 + sets2
#'
#' add_sets
`+.LEV_sets` <- function(lhs, rhs) {
  # Check of the object is proper LEV_sets
  is_LEV <- validate_LEV_sets(lhs, stop_running = TRUE)
  is_LEV <- validate_LEV_sets(rhs, stop_running = TRUE)

  lhs <- as.data.frame(lhs)
  rhs <- as.data.frame(rhs)

  add_sets <- rbind(lhs, rhs)

  class(add_sets) <- "LEV_sets"

  return(add_sets)
}


#' S3 method for printing LEV Profiles Sets
#' @param x Object of class \code{LEV_sets}
#' @param ... Extra arguments
#' @export
#' @examples
#' # Create random athletes/profiles
#' sets <- create_athletes(1) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(70, 80, 90))
#'
#' print(sets)
print.LEV_sets <- function(x, ...) {
  print(stats::coef(x, ...))
}

#' S3 method for extracting LEV Profiles Sets
#' @param object Object of class \code{LEV_sets}
#' @param ... Extra arguments
#' @export
#' @return Data Frame
#' @examples
#' # Create random athletes/profiles
#' sets <- create_athletes(1) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(70, 80, 90))
#'
#' sets_params <- coef(sets)
coef.LEV_sets <- function(object, ...) {
  class(object) <- "list"

  sets <- dplyr::bind_rows(object)

  return(sets)
}


#' S3 method for extracting LEV Profiles Sets
#' @param x Object of class \code{LEV_sets}
#' @param row.names Not used
#' @param optional Not used
#' @param ... Extra arguments
#' @export
#' @return Data Frame
#' @examples
#' # Create random athletes/profiles
#' sets <- create_athletes(1) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(70, 80, 90))
#'
#' sets_params <- as.data.frame(sets)
as.data.frame.LEV_sets <- function(x, row.names = NULL, optional = FALSE, ...) {
  stats::coef(x, ...)
}


#' S3 method for plotting LEV Profiles Sets
#'
#' @details S3 method for plotting LEV Profiles Sets provides plenty of
#'     flexibility and control by using the \code{...} parameters.
#'     User can filter variables, use different x and y scales,
#'     provides colors and facets. Here are the extra parameters your
#'     can utilize:
#'     \describe{
#'      \item{athletes}{Althetes to keep in}
#'      \item{visits}{Visits to keep in}
#'      \item{sets}{Sets to keep in}
#'      \item{reps}{Reps to keep in}
#'      \item{loads}{Loads to keep in}
#'      \item{sets_to_failure}{Sets to failure to keep in}
#'      \item{target_repetitions}{Target repetitions to keep in}
#'      \item{repetitions_done}{Repetitions done to keep in}
#'      \item{RIRs}{Reps-In-Reserve to keep in}
#'      \item{est_RIRs}{Reps-In-Reserve to keep in}
#'      \item{nRMs}{N-Repetition-Maximum loads to keep in}
#'      \item{est_nRMs}{Estimated N-Repetition-Maximum loads to keep in}
#'      \item{failed_reps}{Should failed reps be kept in? Default is \code{FALSE}}
#'      \item{y_var}{Variable on y-axis. Default is 'measured_rep_velocity'}
#'      \item{x_var}{Variable on x-axis. Default is 'load'}
#'      \item{smooth}{Should smoothing be use? Default is \code{TRUE}}
#'      \item{method}{Method for smoothing. Default is 'lm'. See
#'          \code{\link[ggplot2]{geom_smooth}} for more information}
#'      \item{formula}{Equation formula for smoothing. Default is \code{y~x}}
#'      \item{se}{Should confidence interval be provided? Default is \code{TRUE}}
#'      \item{level}{Level of confidence. Default is 0.9 (i.e., 90%)}
#'      \item{color}{What variable should be used to coloring groups? Default is none}
#'      \item{facet}{Variable to be used for plot faceting. Athletes are already faceted,
#'          and this parameter allows for adding extra level. Default is 'visit'}
#'      \item{fullrange}{Should smooth/regression line be extended? Default is \code{FALSE}}
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
#' # Create random athletes/profiles
#' set.seed(1667)
#'
#' sets <- create_athletes(5) %>%
#'   create_visits(1:3) %>%
#'   create_sets(
#'     load = c(90, 110, 130),
#'     load_type = "absolute"
#'   )
#'
#' print(sets)
#'
#' # Extract data frame
#' LEV_data <- as.data.frame(sets)
#' # Or
#' # LEV_data <- coef(sets)
#'
#' plot(sets)
#' plot(sets, athletes = "Athlete 1")
#' plot(sets, athletes = "Athlete 1", reps = 1)
#' plot(sets, athletes = "Athlete 1", x_var = "RIR")
#'
#' # Another way to create LEV profiles
#' sets <- create_profiles(athletes = c("Mladen", "Ivan"), L0 = c(200, 180)) %>%
#'   create_visits(1) %>%
#'   create_sets(load = c(100, 120, 140), load_type = "absolute")
#'
#' plot(sets)
#' plot(sets, facet = NULL, x_var = "load")
#' plot(sets, visits = 1, x_var = "RIR")
plot.LEV_sets <- function(x, type = "athletes", ...) {
  if (!(type %in% c("athletes", "pooled"))) {
    stop("Plot type can be either 'athletes' or 'pooled'", call. = FALSE)
  }

  plot_data <- stats::coef(x)

  plot_LEV_sets(plot_data, type = type, ...)
}

# ===================================
plot_LEV_sets <- function(df,
                          type = "athletes",
                          athletes = NULL,
                          visits = NULL,
                          sets = NULL,
                          reps = NULL,
                          loads = NULL,
                          sets_to_failure = NULL,
                          target_repetitions = NULL,
                          repetitions_done = NULL,
                          nRMs = NULL,
                          est_nRMs = NULL,
                          RIRs = NULL,
                          est_RIRs = NULL,
                          failed_reps = NULL,
                          y_var = "measured_rep_velocity",
                          x_var = "load",
                          smooth = TRUE,
                          method = "lm",
                          formula = y ~ x,
                          se = TRUE,
                          level = 0.9,
                          color = NULL,
                          facet = "visit",
                          fullrange = FALSE) {


  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  athlete <- NULL
  visit <- NULL
  set <- NULL
  set_to_failure <- NULL
  `%1RM` <- NULL
  `RIR` <- NULL
  `nRM` <- NULL
  `est_nRM` <- NULL
  `failed_rep` <- NULL
  est_RIR <- NULL
  reps_done <- NULL
  target_reps <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  y_var_name <- as.name(y_var)
  x_var_name <- as.name(x_var)

  # If null, use visit so it doesn't throw an error
  if (is.null(facet)) {
    facet.tmp <- "visit"
    facet_name <- as.name(facet.tmp)
  } else {
    facet_name <- as.name(facet)
  }

  df <- df %>%
    dplyr::mutate(
      y_var = !!y_var_name,
      x_var = !!x_var_name,
      facet_var = !!facet_name,
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

  if (!is.null(reps)) {
    df <- df %>%
      dplyr::filter(rep %in% reps)
  }

  if (!is.null(loads)) {
    df <- df %>%
      dplyr::filter(load %in% loads)
  }

  if (!is.null(sets_to_failure)) {
    df <- df %>%
      dplyr::filter(set_to_failure %in% sets_to_failure)
  }

  if (!is.null(target_repetitions)) {
    df <- df %>%
      dplyr::filter(target_reps %in% target_repetitions)
  }

  if (!is.null(repetitions_done)) {
    df <- df %>%
      dplyr::filter(reps_done %in% repetitions_done)
  }

  if (!is.null(RIRs)) {
    df <- df %>%
      dplyr::filter(RIR %in% RIRs)
  }

  if (!is.null(est_RIRs)) {
    df <- df %>%
      dplyr::filter(est_RIR %in% est_RIRs)
  }

  if (!is.null(nRMs)) {
    df <- df %>%
      dplyr::filter(nRM %in% nRMs)
  }

  if (!is.null(est_nRMs)) {
    df <- df %>%
      dplyr::filter(est_nRM %in% est_nRMs)
  }

  if (!is.null(failed_reps)) {
    df <- df %>%
      dplyr::filter(failed_rep %in% failed_reps)
  }

  if (is.null(color)) {
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = x_var, y = y_var))
  } else {
    gg <- ggplot2::ggplot(df, ggplot2::aes_string(x = x_var, y = y_var, color = color))
  }

  gg <- gg +
    ggplot2::theme_linedraw() +
    ggplot2::geom_point(alpha = 0.5)

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
    ggplot2::xlab(x_var) +
    ggplot2::ylab(y_var)

  if (smooth) {
    gg <- gg +
      ggplot2::geom_smooth(
        method = method,
        formula = formula,
        se = se,
        level = level,
        fullrange = fullrange
      )
  }

  gg
}
