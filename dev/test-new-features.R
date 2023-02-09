test_sets <- create_profiles(athletes = 1:2, L0_visit_random = 10, L0_fatigue = -10) %>%
  create_visits(1) %>%
  create_tests() %>%
  create_summary()

plot(test_sets, set = "LV")
plot(test_sets, set = "RTF")



