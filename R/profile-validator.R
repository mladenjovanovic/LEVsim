# Validate LEV Profiles
#
# Validates if object is proper class of \code{LEV_profile}
#
# @param LEV_profile Object
# @param stop_running Should R be stopped with a message. Default is \code{TRUE}
#
# @return Logical
#
validate_LEV_profile <- function(LEV_profile, stop_running = TRUE) {
  is_good <- class(LEV_profile) == "LEV_profile"

  if (stop_running) {
    if (!is_good) {
      stop("Object is not proper LEV_profile object. Please use `create_profile()` to create one", call. = FALSE)
    }
  }

  return(is_good)
}
