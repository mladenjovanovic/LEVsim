 set.seed(1667)
 sets <- create_profiles(1, load_increment = 2.5, V0_visit_random = 0.1, L0_visit_random = 1) %>%
   create_visits(1:10) %>%
   create_init_1RM(change_1RM = 0) %>%
   create_sets(load = c(0.7, 0.8), load_type = "prescription 1RM")


 plot(sets, x_var = "visit", y_var = "load", reps = 1, facet = NULL)

 plot(sets, x_var = "visit", y_var = "prescription_1RM", facet = NULL)
 plot(sets, x_var = "visit", y_var = "1RM", facet = NULL)


  set.seed(1667)
  sets <- create_athletes(2) %>%
    create_visits(1:2) %>%
    create_visit_1RM() %>%
    create_sets(load = c(0.7, 0.8, 0.9), load_type = "visit 1RM") %>%
    create_summary()

  sets
  plot(sets)


set.seed(1667)
sets <- create_profiles(1, load_increment = 1) %>%
  create_visits(1:10) %>%
  create_prescription_1RM() %>%
  create_sets(load = c(0.7, 0.8), load_type = "prescription 1RM")

plot(sets, x_var = "visit", y_var = "load", reps = 1, facet = NULL)
plot(sets, x_var = "visit", y_var = "prescription_1RM", facet = NULL)


set.seed(1667)
sets <- create_profiles(1, load_increment = 1) %>%
  create_visits(1:10) %>%
  create_prescription_1RM(120, 2.5) %>%
  create_sets(load = c(0.7, 0.8), load_type = "prescription 1RM")

plot(sets, x_var = "visit", y_var = "load", reps = 1, facet = NULL)
plot(sets, x_var = "visit", y_var = "prescription_1RM", facet = NULL)
