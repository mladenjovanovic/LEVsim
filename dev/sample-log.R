require(STMr)
require(tidyverse)
require(ggdist)
require(directlabels)

#### First create strength training program
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
  phase = 1:4
)

phase_session_A <- expand_grid(phase_df, session_A)
phase_session_B <- expand_grid(phase_df, session_B)

strength_training_program <- rbind(phase_session_A, phase_session_B) %>%
  rename(week = index, target_reps = reps) %>%
  mutate(day = ((phase - 1) * 6) + ((week - 1) * 2) + ifelse(session == "Session A", 1, 2)) %>%
  select(phase, week, day, session, set, target_reps, perc_1RM)

### Create athletes
athletes <- tribble(
  ~athlete,    ~V0, ~L0, ~v1RM, ~V0_rep_drop, ~V0_fatigue, ~L0_rep_drop, ~L0_fatigue, ~L0_visit_change, ~L0_visit_random,  ~V0_visit_change, ~V0_visit_random, ~biological_variation, ~instrumentation_noise,
  "Athlete A", 1.8, 180,  0.2,   0,            0,           0.04,          0,           0,                0,                 0,                0,                0,                    0,
  "Athlete B", 1.8, 180,  0.2,   0,            0,           0.08,          0,           0,                0,                 0,                0,                0,                    0
)

### Create test
RTF_loads <- c(0.9, 0.8, 0.7)
RTF_loads_str <- paste0(round(RTF_loads * 100, 0), "%")

oneRM_load <- c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 0.975, 1, 1.025, 1.05)
oneRM_load_str <- paste0(round(oneRM_load * 100, 1), "%")

create_tests <- function(df) {
  visit <- create_profiles(
    athletes = df$athlete,
    V0 = df$V0,
    L0 = df$L0,
    v1RM = df$v1RM,
    V0_rep_drop = df$V0_rep_drop,
    L0_rep_drop = df$L0_rep_drop,
    biological_variation = df$biological_variation,
    instrumentation_noise = df$instrumentation_noise) %>%
    create_visits() %>%
    create_prescription_1RM(load_perc = seq(0.8, 1.1, by = 0.01))

  LV <- visit %>%
    create_sets(load = oneRM_load, reps = 1, load_type = "prescription 1RM", failed_reps = TRUE) %>%
    as.data.frame() %>%
    mutate(test = "LV")

  RTF <- visit %>%
    create_sets(load = RTF_loads, load_type = "prescription 1RM") %>%
    as.data.frame() %>%
    mutate(test = "RTF")

  rbind(LV, RTF)
}

athlete_tests <- athletes %>%
  rowwise() %>%
  do(create_tests(.)) %>%
  ungroup()

RTF_tests <-  athlete_tests %>%
  filter(test == "RTF") %>%
  mutate(`Load (%1RM)` = factor(load_index, labels = RTF_loads_str))

LV_tests <-  athlete_tests %>%
  filter(test == "LV") %>%
  mutate(`Load (%1RM)` = round(100 *  load / prescription_1RM, 0)) %>%
  filter(failed_rep == FALSE)

# Plot

# LV Profile
LV_tests %>%
  ggplot(aes(x = `Load (%1RM)`, y = measured_rep_velocity)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  #geom_dl(aes(label = paste0(athlete, "  ")), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  ylab("Mean Velocity [m/s]") +
  xlab("Load [%1RM]") +
  facet_wrap(~athlete) +
  theme(legend.position = "none") #+
  #scale_color_brewer(palette = "Dark2")

RTF_tests %>%
  ggplot(aes(x = rep, y = measured_rep_velocity, color = `Load (%1RM)`)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  facet_wrap(~athlete) +
  geom_dl(aes(label = paste0(`Load (%1RM)`, "  ")), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  ylab("Mean Velocity [m/s]") +
  xlab("Repetition") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")

RTF_tests %>%
  ggplot(aes(y = measured_rep_velocity, x = RIR, color = `Load (%1RM)`)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  geom_dl(aes(label = paste0(`Load (%1RM)`, "  ")), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  facet_wrap(~athlete) +
  ylab("MV [m/s]") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")

RTF_tests %>%
  ggplot(aes(y = measured_rep_velocity, x = `%MNR`, color = `Load (%1RM)`)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  facet_wrap(~athlete) +
  ylab("MV [m/s]")  +
  geom_dl(aes(label = paste0(`Load (%1RM)`, "  ")), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")

RTF_tests %>%
  ggplot(aes(y = `%VL`, x = `%MNR`, color = `Load (%1RM)`)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  facet_wrap(~athlete) +
  ylab("Velocity Loss [%]") +
  geom_vline(xintercept = 50, linetype = "dashed", alpha = 0.3) +
  geom_dl(aes(label = paste0(`Load (%1RM)`, "  ")), method = list("last.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  xlim(NA, 110)

#### Get 1RMs
athlete_1RMs <- RTF_tests %>%
  group_by(athlete) %>%
  summarise(`1RM` = prescription_1RM[1]) %>%
  ungroup()

athlete_nRM <- RTF_tests %>%
  group_by(athlete, load) %>%
  summarise(
    `1RM` = prescription_1RM[1],
    `%1RM` = load[1] / prescription_1RM[1],
    nRM = nRM[1],
    start_velocity = measured_rep_velocity[1],
    stop_velocity = measured_rep_velocity[max(rep)],
    best_velocity = max(measured_rep_velocity),
    worst_velocity = min(measured_rep_velocity)) %>%
  ungroup()

athlete_nRM

athlete_nRM %>%
  ggplot(aes(x = `%1RM`, y = nRM, color = athlete)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  xlab("Load [%1RM]") +
  geom_dl(aes(label = paste0(athlete, "  ")), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")

athlete_nRM %>%
  ggplot(aes(x = best_velocity, y = nRM, color = athlete)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  xlab("Best Mean Velocity [m/s]") +
  geom_dl(aes(label = paste0("   ", athlete)), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = 0)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")

#### Create training program

# Prescription 1RM
athlete_presc_1RMs <- athlete_1RMs %>%
  mutate(presc_1RM = LEVsim:::get_load_rounded(0.9 * `1RM`, 2.5)) %>%
  select(-`1RM`)

training_log <- expand_grid(
  athlete_presc_1RMs,
  strength_training_program) %>%
  mutate(presc_1RM = presc_1RM + (phase - 1) * 5) %>%
  # Add athletes data
  left_join(athletes, by = "athlete")

fill_log <- function(df) {
  single_set <- create_profiles(
    athletes = df$athlete[1],
    V0 = df$V0[1],
    L0 = df$L0[1],
    v1RM = df$v1RM[1],
    V0_rep_drop = df$V0_rep_drop[1],
    L0_rep_drop = df$L0_rep_drop[1],
    biological_variation = df$biological_variation[1],
    instrumentation_noise = df$instrumentation_noise[1]) %>%
    create_visits(df$phase[1]) %>%
    create_visit_1RM(load_perc = seq(0.8, 1.1, by = 0.01)) %>%
    create_prescription_1RM(init_1RM = df$presc_1RM[1]) %>%
    create_sets(load = df$perc_1RM, reps = df$target_reps, load_type = "prescription 1RM") %>%
    as.data.frame()

    left_join(
      df %>%
        select(1:9),
      single_set %>%
        select(set, visit_1RM, prescription_1RM, load, nRM, rep, RIR, measured_rep_velocity, `%VL`),
      by = "set")
}

filled_training_log <- training_log %>%
  group_by(athlete, phase, week, day, session) %>%
  do(fill_log(.)) %>%
  ungroup()

summary_training_log <- filled_training_log %>%
  group_by(athlete, phase, week, day, session, set) %>%
  summarise(
    visit_1RM = visit_1RM[1],
    presc_1RM = presc_1RM[1],
    target_reps = target_reps[1],
    perc_1RM = perc_1RM[1],
    load = load[1],
    reps_done = max(rep),
    RIR = RIR[reps_done],
    est_nRM = RIR + reps_done,
    start_velocity = measured_rep_velocity[1],
    stop_velocity = measured_rep_velocity[reps_done],
    best_velocity = max(measured_rep_velocity),
    worst_velocity = min(measured_rep_velocity)) %>%
  ungroup() %>%
  mutate(Phase = factor(phase))


summary_training_log

# training log 1RMs

training_log_1RMs <- summary_training_log %>%
  group_by(athlete, phase, week, day, session) %>%
  summarise(
    visit_1RM = visit_1RM[1],
    presc_1RM = presc_1RM[1],
    ) %>%
  ungroup() %>%
  rename(`Prescription 1RM` = presc_1RM, `Daily 1RM` = visit_1RM) %>%
  pivot_longer(cols = c(`Prescription 1RM`, `Daily 1RM`), names_to = "1RM") %>%
  mutate(Phase = factor(phase))

# Plot
training_log_1RMs %>%
  ggplot(aes(x = day, y = value, color = `1RM`)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  facet_wrap(~athlete) +
  xlab("Day") +
  ylab("1RM [kg]") +
  scale_color_brewer(palette = "Dark2") +
  geom_dl(aes(label = `1RM`), method = list("first.qp", cex = 0.75,  hjust = 0, vjust = 1.5)) +
  theme(legend.position = "none")

summary_training_log %>%
  ggplot(aes(x = load, y = est_nRM, color = Phase)) +
  theme_ggdist() +
  geom_point(alpha = 0.3) +
  facet_wrap(~athlete) +
  xlab("Load [kg]") +
  ylab("Estimated nRM") +
  geom_smooth(se = FALSE, linewidth = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30))

summary_training_log %>%
  ggplot(aes(x = perc_1RM, y = est_nRM, color = Phase)) +
  theme_ggdist() +
  geom_point(alpha = 0.3) +
  facet_wrap(~athlete) +
  geom_smooth(se = FALSE, linewidth = 0.5) +
  xlab("Prescription 1RM [%]") +
  ylab("Estimated nRM") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30))

