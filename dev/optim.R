set.seed(1667)
sets <- create_profiles(1,  V0 = 2, L0 = 180, v1RM =  0.2, V0_rep_drop =  0, L0_rep_drop =  0.04) %>%
  create_visits(1) %>%
  create_visit_1RM() %>%
  create_sets(load = c(0.5, 0.6, 0.7, 0.8, 0.9), load_type = "visit 1RM")

plot(sets)

target_sets <- as.data.frame(sets) %>%
  select(set, load, rep, RIR, measured_rep_velocity)

target_load <- target_sets %>%
  group_by(set) %>%
  summarise(load = load[[1]])

model_sets <- function(V0, L0, v1RM, V0_rep_drop, L0_rep_drop, load) {
  create_profiles(1,  V0 = V0, L0 = L0, v1RM = v1RM, V0_rep_drop = V0_rep_drop, L0_rep_drop = L0_rep_drop) %>%
    create_visits(1) %>%
    create_sets(load = load)
}

model_MSE <- function(predicted_sets, target_sets) {
  if (any(is.na(predicted_sets$RTF))) {
    browser()
  }
  predicted_sets <- as.data.frame(predicted_sets) %>%
    select(set, load, rep, RIR, measured_rep_velocity) %>%
    rename(predicted_measured_rep_velocity = measured_rep_velocity)

  df <- left_join(target_sets, predicted_sets, by = c("set", "load", "RIR"))
  df <- na.omit(df)
  browser()
  mean((df$measured_rep_velocity - df$predicted_measured_rep_velocity)^2)
}

optimx::optimx(c(1.8, 150, 0.1, 0, 0.05), function(x) {
  print(x)
  model_MSE(model_sets(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], target_load$load), target_sets)
}, lower = c(0.5, 100, 0.1, 0, 0.01), upper = c(3, 200, 0.7, 0.1, 0.1), control = list(trace = TRUE))

model_MSE(model_sets(1.8, 180, 0.1, 0, 0.04, target_load$load), target_sets)

model_MSE(model_sets(1.8, 150, 0.1, 0, 0.0452, target_load$load), target_sets)
model_MSE(model_sets(1.8, 150, 0.1, 0, 0.0452, target_load$load), target_sets)
