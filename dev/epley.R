require(tidyverse)
require(LEVsim)
require(STMr)

params <- expand_grid(
  L0 = 200,
  V0 = 1.8,
  v1RM = 0.2
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
    V0_rep_drop = x[1],
    L0_rep_drop = x[2])

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
  par = c(0.05, 0.05),
  RTF_func,
  method = "L-BFGS-B",
  lower = c(0, 0.03),
  upper = c(0.2, 0.1),
  control = list(trace = 100))

stop()


# Only L0
0.04476617
0.04451621

# V0 and L0
7.018543e-05
4.547028e-02

# New shape of the curve

# Only additive / deccelerating
0.04244762 # par
20.47 # % error

# Only multiplicative / accelerating
0.05345957 # par
16.70027 # error

# Only linear
0.04548258 # param
16.98229 # res

# Latest linear
0.0417001 # par

# Both

# Plot
create_profiles(
  athlete = params$athlete,
  L0 = params$L0,
  V0 = params$V0,
  v1RM = params$v1RM,

  # fatigue effect
  V0_rep_drop = 0.038, #0.03446838,
  L0_rep_drop = 0.038) %>%#0.03781305) %>%
  create_visits(1) %>%
  create_visit_1RM() %>%
  create_sets(load = loads_perc, load_type = "visit 1RM", max_reps = 200) %>%
  create_summary() %>%
  as.data.frame() %>%
  select(athlete, load_index, load_perc, nRM) %>%
  mutate(
    pred_nRM = STMr::max_reps_epley(load_perc),
    diff = pred_nRM - nRM,
    diff_perc = diff / nRM) %>%
  na.omit() %>%
  ggplot(aes(x = load_perc, y = nRM, group = athlete)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = pred_nRM), color = "red")
