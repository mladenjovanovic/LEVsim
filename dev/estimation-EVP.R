require(tidyverse)
require(LEVsim)

# Create RTF data
RTF_df <- create_profiles(biological_variation_multiplicative = 0.1, L0_rep_drop = 0.03) %>%
  create_visits() %>%
  create_tests() %>%
  as.data.frame() %>%
  filter(set == "RTF")

# Plot
RTF_df %>%
  ggplot(aes(x = rep, y = measured_rep_velocity, color = factor(load))) +
  geom_point() +
  geom_line()

# Create model --------------------------------------------------
m1 <- estimate_EVP(
  rep = RTF_df$rep,
  load = RTF_df$load,
  velocity = RTF_df$measured_rep_velocity, model_weights = RTF_df$load)


m1$data %>%
  pivot_longer(cols = c(velocity, pred_velocity), names_to = "type") %>%
  ggplot(aes(x = rep, y = value, color = factor(load))) +
  geom_point() +
  geom_line(aes(linetype = type))
