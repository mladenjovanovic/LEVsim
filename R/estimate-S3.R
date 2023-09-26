#' S3 method for printing \code{LEV_estimate} object
#' @param x \code{LEV_estimate} object
#' @param ... Not used
#' @export
#' @examples
#' LV_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "LV")
#'
#' m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)
#'
#' m1
print.LEV_estimate <- function(x, ...) {
  cat("Estimated Parameters\n")
  cat("--------------------\n")
  print(data.frame(x$parameters), row.names = FALSE)
}

#' S3 method for providing summary for the \code{LEV_estimate} object
#' @param object \code{LEV_estimate} object
#' @param ... Not used
#' @export
#' @examples
#' LV_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "LV")
#'
#' m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)
#'
#' summary(m1)
summary.LEV_estimate <- function(object, ...) {
  cat("Estimated Parameters\n")
  cat("--------------------\n")
  print(data.frame(object$parameters), row.names = FALSE)

  cat("\nModel fit estimators\n")
  cat("--------------------\n")
  print(data.frame(object$model_fit), row.names = FALSE)

  if(!is.null(object$model))  {
    cat("\nModel summary\n")
    cat("-------------\n")
    summary(object$model, ...)
  }
}

#' S3 method for plotting the \code{LEV_estimate} object
#' @param x \code{LEV_estimate} object
#' @param ... Not used
#' @return \link[ggplot2]{ggplot} object
#' @export
#' @examples
#' LV_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "LV")
#'
#' m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)
#'
#' plot(m1)
plot.LEV_estimate <- function(x, ...) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  Fitted <- NULL
  Residuals <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  data.frame(Fitted = stats::fitted(x), Residuals = stats::resid(x)) %>%
    ggplot2::ggplot(ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::theme_linedraw() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_point(shape = 1, alpha = 0.8)
}


#' S3 method for providing fitted/predicted observations for the \
#'     code{LEV_estimate} object
#' @param object \code{LEV_estimate} object
#' @param ... Not used
#' @examples
#' LV_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "LV")
#'
#' m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)
#'
#' fitted(m1)
#' @export
fitted.LEV_estimate <- function(object, ...) {
  object$data %>%
    dplyr::pull(dplyr::starts_with("pred_"))
}



#' S3 method for providing residuals for the \code{LEV_estimate} object
#' @param object \code{LEV_estimate} object
#' @param ... Not used
#' @examples
#' LV_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "LV")
#'
#' m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)
#'
#' residuals(m1)
#' @export
residuals.LEV_estimate <- function(object, ...) {
  object$data$residuals
}

#' S3 method for extracting model parameters from \code{LEV_estimate} object
#' @param object \code{LEV_estimate} object
#' @param ... Not used
#' @examples
#' LV_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "LV")
#'
#' m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)
#'
#' coef(m1)
#' @export
coef.LEV_estimate <- function(object, ...) {
  data.frame(object$parameters)
}

#' S3 method for making predictions using \code{LEV_estimate}
#'
#' @param object \code{LEV_estimate} object
#' @param ... Forwarded to generic \code{predict()} function
#' @examples
#' LV_df <- create_profiles() %>%
#'   create_visits() %>%
#'   create_tests() %>%
#'   as.data.frame() %>%
#'   dplyr::filter(set == "LV")
#'
#' m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)
#'
#' predict(m1, newdata = data.frame(load = 100))
#' @export
predict.LEV_estimate <- function(object, ...) {
  if(!is.null(object$model)) {
    stats::predict(object$model, ...)
  } else {
    NULL
  }
}
