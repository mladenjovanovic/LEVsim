require(tidyverse)
require(LEVsim)

# Create RTF data
RTF_df <- create_profiles() %>%
  create_visits() %>%
  create_tests() %>%
  create_summary() %>%
  as.data.frame() %>%
  filter(set == "RTF")

m1 <- estimate_RTF(
  RTF_df$load,
  RTF_df$nRM)

m1


# Plot
RTF_df %>%
  ggplot(aes(x = load, y = nRM)) +
  geom_point()

RTF_df %>%
  ggplot(aes(x = load_perc_adj * 100, y = nRM)) +
  geom_point()

# Create model --------------------------------------------------
test <- estimate_RTF(RTF_df$load_perc, RTF_df$nRM, return_multiple = TRUE, relative_load = TRUE)

test$parameters
test$model_fit

