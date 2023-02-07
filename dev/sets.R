set.seed(1667)
sets <- create_profiles(1, biological_variation = 0.2, v1RM = 0.1) %>%
 create_visits(1:2) %>%
 create_visit_1RM() %>%
 create_sets(load = rep(0.8, 10), load_type = "visit 1RM", L0_fatigue = 0.01, V0_fatigue = 0) %>%
 create_summary()

plot(sets)


sets <- as.data.frame(sets)
as.data.frame(sets)

plot(sets, x_var = "visit", y_var = "load", reps = 1, facet = NULL)
plot(sets, x_var = "visit", y_var = "visit_1RM", facet = NULL)

y <- as.data.frame(x)
plot(sets, x_var = "visit", y_var = "prescription_1RM", facet = NULL)



x <- create_profiles(1) %>%
  create_visits(1)

get_sets(x$`1`$visit$`1`, c(150, 150, 150))


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

set.seed(1667)

sets <- create_athletes(5) %>%
  create_visits(1:3) %>%
  create_prescription_1RM(100, 2.5) %>%
  create_sets(
     load = c(70, 80, 90),
     load_type = "prescription 1RM"
   )
y <- as.data.frame(sets)
print(sets)

plot(sets, reps = 1)
