## code to prepare `strength_training_program` dataset goes here
require(STMr)
require(tidyverse)

session_A <- scheme_wave(
  reps = c(12, 10, 8, 12, 10, 8),
  # Adjusting using lower %1RM (RIR Increment method used)
  adjustment = c(4, 2, 0, 6, 4, 2),
  vertical_planning = vertical_linear,
  vertical_planning_control = list(reps_change = c(0, -2, -4)),
  progression_table = progression_RIR_increment,
  progression_table_control = list(volume = "extensive")
) %>% mutate(
  session = "Session A"
)

session_B <- scheme_wave(
  reps = c(3, 2, 1, 3, 2, 1),
  # Adjusting using lower %1RM (RIR Increment method used)
  adjustment = c(6, 4, 2, 4, 2, 0),
  vertical_planning = vertical_constant,
  vertical_planning_control = list(n_steps = 3),
  progression_table = progression_RIR_increment,
  progression_table_control = list(volume = "normal")
) %>% mutate(
  session = "Session B"
)

phase_df <- tibble(
  phase = 1:6
)

phase_session_A <- expand_grid(phase_df, session_A)
phase_session_B <- expand_grid(phase_df, session_B)

strength_training_program <- rbind(phase_session_A, phase_session_B) %>%
  rename(week = index, target_reps = reps) %>%
  mutate(visit = ((phase - 1) * 6) + ((week - 1) * 2) + ifelse(session == "Session A", 1, 2)) %>%
  select(phase, week, visit, session, set, target_reps, perc_1RM) %>%
  arrange(phase, week, visit, session)

usethis::use_data(strength_training_program, overwrite = TRUE)
