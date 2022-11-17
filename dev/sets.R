x <- create_profiles(1) %>%
  create_visits(1) %>%
  create_sets(load = c(120, 130, 140, 150, 160), load_type = "absolute")

y <- as.data.frame(x)
plot(x, x_var = "RIR")

plot(x, visits = 1, facet = "trial", reps = 1)

y <- get_sets(x$`Athlete 1`$visit$`1`, 120)


system.time({get_sets(x$`Athlete 1`$visit$`1`, rep(100, 1000))})


#' # Create random athletes/profiles
#'  set.seed(1667)
#'
#'  sets <- create_athletes(5) %>%
#'   create_visits(1:3) %>%
#'   create_sets(
#'     load = c(90, 110, 130),
#'     load_type = "absolute"
#'   )
#'
#' print(sets)
#'
#'# Extract data frame
#' LEV_data <- as.data.frame(sets)
#' # Or
#' # LEV_data <- coef(sets)
#'
#' plot(sets)
#' plot(sets, athletes = "Athlete 1")
#' plot(sets, athletes = "Athlete 1", reps = 1)
#' plot(sets, athletes = "Athlete 1", x_var = "RIR")
#'
#'  # Another way to create LEV profiles
#'  sets <- create_profiles(athletes = c("Mladen", "Ivan"), L0 = c(200, 180)) %>%
#'    create_visits(1) %>%
#'    create_sets(load = c(100, 120, 140), load_type = "absolute")
#'
#'  plot(sets)
#'  plot(sets, facet = NULL, x_var = "load")
#'  plot(sets, visits = 1, x_var = "RIR")





