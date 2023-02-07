set.seed(1667)
sets <- create_profiles(1, load_increment = 2.5, v1RM = 0.3, biological_variation = 0.2) %>%
  create_visits(1:2) %>%
  #create_prescription_1RM() %>%
  create_sets(load = c(120, 120, 120, 120), use_true_velocity = FALSE) %>%
  create_summary()

plot(sets) +
  ggplot2::geom_hline(yintercept = 0.3)
