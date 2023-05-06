require(tidyverse)
require(LEVsim)
require(STMr)

params <- expand_grid(
  L0 = seq(120, 250, length.out = 4),
  V0 = seq(1.5, 2.5, length.out = 4),
  v1RM = seq(0.05, 0.4, length.out = 4)
) %>%
  mutate(athlete = seq(1, n()))

loads_perc <- c(70, 75, 80, 85, 90, 95) / 100

RTF_func <- function(x) {
  profiles <- create_profiles(
    athlete = params$athlete,
    L0 = params$L0,
    V0 = params$V0,
    v1RM = params$v1RM,

    # fatigue effects
    V0_rep_drop_multiplicative = x[1],
    L0_rep_drop_multiplicative = x[2])

  RTF <- profiles %>%
    create_visits(1) %>%
    create_visit_1RM() %>%
    create_sets(load = loads_perc, load_type = "visit 1RM", max_reps = 200) %>%
    create_summary() %>%
    as.data.frame() %>%
    select(athlete, load_index, nRM) %>%
    mutate(
      perc_1RM = loads_perc[load_index],
      pred_nRM = STMr::max_reps_epley(perc_1RM),
      diff = pred_nRM - nRM,
      diff_perc = diff / nRM) %>%
    na.omit()

  RMSE <- 100 * sqrt(mean(RTF$diff_perc^2))
  cat(x, RMSE, "\n")
  RMSE
}

res <- optim(
  par = c(0, 0.05),
  RTF_func,
  method = "L-BFGS-B",
  lower = c(0.0, 0.01),
  upper = c(0.1, 0.1),
  control = list(trace = TRUE))

stop()
# Manual
man <- tibble(
  param = seq(2, 6, length.out = 30) / 100
) %>%
  rowwise() %>%
  mutate(
    RMSE = RTF_func(param)
  )

ggplot(man, aes(x = param, y = RMSE)) +
  theme_linedraw() +
  geom_point() +
  geom_line()

RTF_func(c(0.04476617))

# Only L0
0.04476617
0.04451621

# V0 and L0
7.018543e-05 4.547028e-02
