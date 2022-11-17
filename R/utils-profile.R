# Get Velocity at Load
#
# Function returns velocity at specific load given the LEV profile
#
# @param V0 Numeric
# @param L0 Numeric
# @param load Numeric
#
# @return Numeric
get_velocity_at_load <- function(V0, L0, load) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  intercept <- NULL
  slope <- NULL
  velocity <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Extra checks to be positive number
  V0 <- ifelse(V0 < 0, 0, V0)
  L0 <- ifelse(L0 < 0, 0, L0)

  df <- dplyr::tibble(
    V0 = V0,
    L0 = L0,
    load = load,
    intercept = V0,
    slope = ifelse(L0 == 0, -Inf, -V0 / L0),
    velocity = intercept + slope * load
  ) %>%
    # Clean up velocity outside profile
    dplyr::mutate(
      velocity_clean = dplyr::if_else(
        (velocity >= 0) & (velocity <= V0),
        velocity,
        dplyr::if_else(
          velocity < 0,
          0,
          V0
        )
      )
    )

  return(df$velocity_clean)
}

# Get Load at Velocity
#
# Function returns load at specific velocity given the LEV profile
#
# @param V0 Numeric
# @param L0 Numeric
# @param velocity Numeric
#
# @return Numeric
get_load_at_velocity <- function(V0, L0, velocity) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  intercept <- NULL
  slope <- NULL
  load <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Extra checks to be positive number
  V0 <- ifelse(V0 < 0, 0, V0)
  L0 <- ifelse(V0 < 0, 0, L0)

  df <- dplyr::tibble(
    V0 = V0,
    L0 = L0,
    velocity = velocity,
    intercept = L0,
    slope = ifelse(V0 == 0, -Inf, -L0 / V0),
    load = intercept + slope * velocity
  ) %>%
    # Clean up load outside profile
    dplyr::mutate(
      load_clean = dplyr::if_else(
        (load >= 0) & (load <= L0),
        load,
        dplyr::if_else(
          load < 0,
          0,
          L0
        )
      )
    )

  return(df$load_clean)
}
