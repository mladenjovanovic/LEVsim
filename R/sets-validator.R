# Validate LEV Sets
#
# Validates if object is proper class of \code{LEV_sets}
#
# @param LEV_sets Object
# @param stop_running Should R be stopped with a message. Default is \code{TRUE}
#
# @return Logical
#
validate_LEV_sets <- function(LEV_sets, stop_running = TRUE) {
  is_good <- class(LEV_sets) == "LEV_sets"

  if (stop_running) {
    if (!any(is_good)) {
      stop("Object is not proper LEV_sets object. Please use `create_sets()` to create one", call. = FALSE)
    }
  }

  return(is_good)
}
