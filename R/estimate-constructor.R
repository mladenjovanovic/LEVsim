new_profile_estimate <- function(parameters, model_fit, model, data) {
  profile_estimate_object <- list(
    data = data,
    model = model,
    parameters = parameters,
    model_fit = model_fit
  )

  class(profile_estimate_object) <- "LEV_estimate"
  return(profile_estimate_object)
}
