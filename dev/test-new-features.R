require(tidyverse)
require(ggdist)
require(directlabels)

data("strength_training_program")

set.seed(1)
athlete_profiles <- create_athletes(2)
plot(athlete_profiles) +
  theme_ggdist()

# Create LV and RTF tests
RTF_loads <- c(0.9, 0.8, 0.7)
RTF_loads_str <- paste0(round(RTF_loads * 100, 0), "%")

LV_loads <- c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 0.975, 1)
LV_str <- paste0(round(LV_loads * 100, 1), "%")


tests <- athlete_profiles %>%
  create_visits(1) %>%
  create_tests(load_LV = LV_loads, load_RTF = RTF_loads, failed_reps = TRUE, failed_sets = TRUE)

# Native plot
plot(tests, facet = "set", smooth = FALSE, color = "failed_rep") +
  theme_ggdist() +
  geom_hline(aes(yintercept = v1RM), linetype = "dashed", alpha = 0.5) +
  labs(color ='Failed Rep') +
  scale_color_manual(values = c("black", "red"))

stop()
# Filter out the tests
tests <- as.data.frame(tests)

RTF_tests <-  tests %>%
  filter(set == "RTF") %>%
  mutate(`Load (%1RM)` = factor(RTF_loads_str[load_index]))

LV_tests <-  tests %>%
  filter(set == "LV") %>%
  mutate(`Load (%1RM)` = round(100 *  load / visit_1RM, 0))

# =====================================
# Tests
LV_tests %>%
  ggplot(aes(x = `Load (%1RM)`, y = measured_rep_velocity)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  #geom_dl(aes(label = paste0(athlete, "  ")), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  ylab("Mean Velocity [m/s]") +
  xlab("Load [%1RM]") +
  facet_wrap(~athlete) +
  theme(legend.position = "none") +
  geom_hline(aes(yintercept = v1RM), linetype = "dashed", alpha = 0.5)
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
  scale_color_brewer(palette = "Dark2") +
  geom_hline(aes(yintercept = v1RM), linetype = "dashed", alpha = 0.5)

RTF_tests %>%
  ggplot(aes(y = measured_rep_velocity, x = RIR, color = `Load (%1RM)`)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  geom_dl(aes(label = paste0(`Load (%1RM)`, "  ")), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  facet_wrap(~athlete) +
  ylab("MV [m/s]") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  geom_hline(aes(yintercept = v1RM), linetype = "dashed", alpha = 0.5)

RTF_tests %>%
  ggplot(aes(y = measured_rep_velocity, x = `%MNR`, color = `Load (%1RM)`)) +
  theme_ggdist() +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  facet_wrap(~athlete) +
  ylab("MV [m/s]")  +
  geom_dl(aes(label = paste0(`Load (%1RM)`, "  ")), method = list("first.bumpup", cex = 0.75,  hjust = 0, vjust = -1)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  geom_hline(aes(yintercept = v1RM), linetype = "dashed", alpha = 0.5)

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
  summarise(`1RM` = visit_1RM[1]) %>%
  ungroup()

athlete_nRM <- RTF_tests %>%
  group_by(athlete, load) %>%
  summarise(
    `1RM` = visit_1RM[1],
    `%1RM` = load[1] / visit_1RM[1],
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
stop()
# Prescription 1RM
require(tictoc)

filled_training_log <- athlete_profiles %>%
  create_visits(1:36) %>%
  #create_visit_1RM() %>%
  create_prescription_1RM(buffer = 0.9, visit_1RM_func = function(init_1RM, visit) {init_1RM + 2.5 * ((visit - 1) %/% 6)}) %>%
  create_program_sets(
    program_df = strength_training_program %>%
      slice_tail(n=1),
    visit = "visit",
    load = "perc_1RM",
    reps = "target_reps",
    load_type = "prescription 1RM")

x <- filled_training_log %>%
  create_summary() %>%
  plot()#as.data.frame()


summary_training_log <- filled_training_log %>%
  group_by(athlete, phase, week, visit, session, set) %>%
  summarise(
    visit_1RM = visit_1RM[1],
    prescription_1RM = prescription_1RM[1],
    target_reps = target_reps[1],
    perc_1RM = perc_1RM[1],
    load = load[1],
    reps_done = max(rep),
    RIR = RIR[reps_done],
    est_RIR = est_RIR[reps_done],
    est_nRM = est_nRM[reps_done],
    `%MNR` = `%MNR`[reps_done],
    `est_%MNR` = `est_%MNR`[reps_done],
    start_velocity = measured_rep_velocity[1],
    stop_velocity = measured_rep_velocity[reps_done],
    best_velocity = max(measured_rep_velocity),
    worst_velocity = min(measured_rep_velocity),
    `%VL` = `%VL`[reps_done]) %>%
  ungroup() %>%
  mutate(Phase = factor(phase))


summary_training_log

# training log 1RMs
training_log_1RMs <- summary_training_log %>%
  group_by(athlete, phase, week, visit, session) %>%
  summarise(
    visit_1RM = visit_1RM[1],
    prescription_1RM = prescription_1RM[1],
  ) %>%
  ungroup() %>%
  rename(`Prescription 1RM` = prescription_1RM, `Daily 1RM` = visit_1RM) %>%
  pivot_longer(cols = c(`Prescription 1RM`, `Daily 1RM`), names_to = "1RM") %>%
  mutate(Phase = factor(phase))

# Plot
training_log_1RMs %>%
  ggplot(aes(x = visit, y = value, color = `1RM`)) +
  theme_ggdist() +
  geom_step(alpha = 0.8) +
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

# Plot RIR and eRIR
summary_training_log %>%
  ggplot(aes(x = est_RIR, y = RIR)) +
  theme_ggdist() +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, linetype = "dashed") +
  facet_wrap(~athlete) +
  scale_color_brewer(palette = "Dark2")

# Plot %MNR and e%MNR
summary_training_log %>%
  ggplot(aes(x = `est_%MNR`, y = `%MNR`)) +
  theme_ggdist() +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, linetype = "dashed") +
  facet_wrap(~athlete) +
  scale_color_brewer(palette = "Dark2")

# Plot Velocity est_nRM
summary_training_log %>%
  ggplot(aes(x = load, y = best_velocity, color = Phase)) +
  theme_ggdist() +
  geom_point(alpha = 0.3) +
  facet_wrap(~athlete) +
  geom_smooth(se = FALSE, linewidth = 0.5, method = lm) +
  scale_color_brewer(palette = "Dark2")

summary_training_log %>%
  ggplot(aes(x = best_velocity, y = est_nRM, color = Phase)) +
  theme_ggdist() +
  geom_point(alpha = 0.3) +
  facet_wrap(~athlete) +
  geom_smooth(se = FALSE, linewidth = 0.5, method = lm) +
  scale_color_brewer(palette = "Dark2")
