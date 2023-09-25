require(tidyverse)
require(LEVsim)

# Create LV data
LV_df <- create_profiles(biological_variation_multiplicative = 0.05) %>%
  create_visits() %>%
  create_tests() %>%
  as.data.frame() %>%
  filter(set == "LV")

# Plot
LV_df %>%
  ggplot(aes(x = load, y = measured_rep_velocity)) +
  geom_point()

# Create model --------------------------------------------------
test <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)

test$parameters

LV_df <- create_profiles() %>%
  create_visits() %>%
  create_tests() %>%
  as.data.frame() %>%
  filter(set == "LV")

m1 <- estimate_LVP(LV_df$load, LV_df$measured_rep_velocity)

m1
