# Create Systematic Effects
#
# Auxiliary function to create systematic effects on LEV profiles
#
# @param x Numeric
# @param visit Numeric vector
# @param effect Effect size
# @param multiplicative Logical
#
# @return Numeric vector
systematic_effect <- function(x, visit = 0, effect = 0, multiplicative = FALSE) {
  df <- dplyr::tibble(
    x = x,
    visit = visit,
    effect = effect,
    multiplicative
  )

  with(df, dplyr::if_else(
    multiplicative == TRUE,
    x * (effect^visit),
    x + (visit * effect)
  ))
}

# Create Random Effects
#
# Auxiliary function to create random effects on LEV profiles
#
# @param x Numeric
# @param visit Numeric vector
# @param effect Effect size
# @param multiplicative Logical
#
# @return Numeric vector
random_effect <- function(x, visit = 0, effect = 0, multiplicative = FALSE) {
  df <- dplyr::tibble(
    x = x,
    visit = visit,
    effect = effect,
    multiplicative
  )

  n_visits <- nrow(df)

  with(df, dplyr::if_else(
    multiplicative == TRUE,
    x * stats::rnorm(n = n_visits, mean = 1, sd = effect),
    x + stats::rnorm(n = n_visits, mean = 0, sd = effect)
  ))
}
