set.seed(1667)
sets <- create_profiles(
  1,
  load_increment = 2.5,
  v1RM = 0.3,
  biological_variation = 0.1,
  L0_fatigue = 0,
  L0_fatigue_multiplicative = FALSE,
  L0_visit_random = 0) %>%
  create_visits(1:3) %>%
  #create_prescription_1RM() %>%
  create_sets(load = rep(120, 10),  reps = 5, use_true_velocity = FALSE) %>%
  create_summary()


plot(sets) +
  ggplot2::geom_hline(yintercept = 0.3)

x <- as.data.frame(sets)
