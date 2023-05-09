x <- get_reps_velocity(V0 = 2,
                              V0_rep_drop = 0.03,
                              L0 = 200,
                              L0_rep_drop = 0.05, #0.05,
                              biological_variation_additive = 0,
                              biological_variation_multiplicative = 0,
                              instrumentation_noise_additive = 0,
                              instrumentation_noise_multiplicative = 0,
                              rep = 1:20,
                              load = 10)

x %>%
  ggplot(aes(x = factor(rep), y = rep_V0, group = 1)) +
  geom_line() +
  geom_point(size = 2,
             shape = 21, fill = "white")

