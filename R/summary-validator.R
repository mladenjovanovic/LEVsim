# Validate LEV Summary
#
# Validates if object is proper class of \code{LEV_summary}
#
# @param LEV_summary Object
# @param stop_running Should R be stopped with a message. Default is \code{TRUE}
#
# @return Logical
#
validate_LEV_summary <- function(LEV_summary, stop_running = TRUE) {
  is_good <- class(LEV_summary) == "LEV_summary"

  if (stop_running) {
    if (!is_good) {
      stop("Object is not proper LEV_summary object. Please use `create_summary()` to create one", call. = FALSE)
    }
  }

  return(is_good)
}
