# First Stage Boosting

library("tidymodels")
library("janitor")
library("tidyverse")
library("rvest")
library("xgboost")
library("rpart.plot")
library("vip")
library("flextable")
library("here")

set_flextable_defaults(
  font.size = 10, theme_fun = theme_apa,
  padding = 3,
  background.color = "#EFEFEF",
  text.align = "center")

team0 <- read_csv(here("Data/full_team.csv")) %>%
  select(-`...1`)

team <- team0 %>% filter(ineligible !=1) %>%
  select(-ap, -team_id, -conf_abbrev, -wins, -losses, -nc_wins, -nc_losses, -sag, -ineligible, -conf_classification) %>%
  mutate(make_tournament = factor(make_tournament)) %>%
  filter(season > 2016) %>%
  mutate(index = if_else(season < 2020, season - 2016, season - 2017))

boost_tuning_grid <- grid_regular(
  tree_depth(range = c(3, 6)),
  levels = 10)

s1_total_pred <- tibble()
for(i in 1:7){
  team_train <- team %>% filter(index != i)
  team_test <- team %>% filter(index == i)
  set.seed(1128)
  team_folds <- vfold_cv(team_train, v = 5, strata = make_tournament)
  
  class_boost_spec <- boost_tree(trees = 1500, tree_depth = tune()) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
  s1_recipe <- recipe(make_tournament ~ ., data = team_train) %>%
    step_rm(season, team_name, seed) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())
  
  # fine tune the model
  s1_boost_tune_results <- tune_grid(
    object = workflow() %>%
      add_recipe(s1_recipe) %>%
      add_model(class_boost_spec),
    resamples = team_folds,
    grid = boost_tuning_grid,
    metrics = metric_set(accuracy))
  
  s1_boost_best_params <- select_best(s1_boost_tune_results, "accuracy")
  
  s1_boost_final_fit <- finalize_workflow(
    workflow() %>%
      add_recipe(s1_recipe) %>%
      add_model(class_boost_spec),
    s1_boost_best_params) %>%
    fit(data = team_train)
  
  s1_boost_predictions <- augment(s1_boost_final_fit, new_data = team_test)
  
  s1_total_pred <- bind_rows(s1_total_pred, s1_boost_predictions)
}

s1_final_predictions <- s1_total_pred %>%
  arrange(season, desc(.pred_1)) %>%
  group_by(season) %>%
  mutate(
    pred_make_tournament = if_else(rank(.pred_0) < 69, 1, 0))

s1_final_predictions %>%
  select(team_name, season, make_tournament, contains("pred")) %>%
  filter(make_tournament != pred_make_tournament) %>%
  select(-.pred_class, -.pred_0) %>% view()

s1_final_predictions %>%
  filter(make_tournament != pred_make_tournament) %>%
  summarise(
    misclassifications = sum(make_tournament == "1")) %>%
  rename_with(~str_to_title(.)) %>%
  flextable() %>%
  align(align = "center") %>%
  colformat_double(j = 1, digits = 0)

vip(s1_boost_final_fit)

# Boosting Order ----------------------------------------------------------

order <- s1_final_predictions %>%
  select(-.pred_1, -.pred_0, -.pred_class)

s2_total_pred <- tibble()
for(i in 1:7){
  order_train <- order %>% filter(make_tournament == 1, index != i) %>% select(-contains("tournament"))
  order_test <- order %>% filter(pred_make_tournament == 1, index == i) %>% select(-contains("tournament"))
  set.seed(1128)
  order_folds <- vfold_cv(order_train, v = 5, strata = seed)
  
  reg_boost_spec <- boost_tree(trees = 1500, tree_depth = tune()) %>%
    set_engine("xgboost") %>%
    set_mode("regression")
  
  s2_recipe <- recipe(seed ~ ., data = order_train) %>%
    step_rm(season, team_name, index) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())
  
  # fine tune the model
  s2_boost_tune_results <- tune_grid(
    object = workflow() %>%
      add_recipe(s2_recipe) %>%
      add_model(reg_boost_spec),
    resamples = order_folds,
    grid = boost_tuning_grid,
    metrics = metric_set(rmse))
  
  s2_boost_best_params <- select_best(s2_boost_tune_results, metric = "rmse")
  
  s2_boost_final_fit <- finalize_workflow(
    workflow() %>%
      add_recipe(s2_recipe) %>%
      add_model(reg_boost_spec),
    s2_boost_best_params) %>%
    fit(data = order_train)
  
  s2_boost_predictions <- augment(s2_boost_final_fit, new_data = order_test)
  
  s2_total_pred <- bind_rows(s2_total_pred, s2_boost_predictions)
}

predicted_seeds <- s2_total_pred %>% 
  arrange(season, .pred) %>%
  group_by(season) %>%
  mutate(
    .pred_a = if_else(conf_result == "champion", NA, .pred),
    play_in = case_when(
      rank(-.pred_a) < 3 & (rank(-.pred_a)+1)%%2 == 0 ~ "anchor2",
      rank(-.pred_a) < 3 & (rank(-.pred_a)+1)%%2 == 1 ~ "float2",
      rank(-.pred_a) < 5 & (rank(-.pred_a)+1)%%2 == 0 ~ "anchor1",
      rank(-.pred_a) < 5 & (rank(-.pred_a)+1)%%2 == 1 ~ "float1",
      .default = NA),
    .pred_c = case_when(
      str_detect(play_in, "float") ~ NA,
      .default = .pred),
    rank_c = rank(.pred_c),
    pred_seed_a = case_when(
      rank_c > 60 ~ 16,
      rank_c > 56 ~ 15,
      rank_c > 52 ~ 14,
      rank_c > 48 ~ 13,
      rank_c > 44 ~ 12,
      rank_c > 40 ~ 11,
      rank_c > 36 ~ 10,
      rank_c > 32 ~ 9,
      rank_c > 28 ~ 8,
      rank_c > 24 ~ 7,
      rank_c > 20 ~ 6,
      rank_c > 16 ~ 5,
      rank_c > 12 ~ 4,
      rank_c > 8 ~ 3,
      rank_c > 4 ~ 2,
      .default = 1),
    pred_seed_b = if_else(is.na(play_in), NA, pred_seed_a),
    pred_seed = case_when(
      play_in == "float1" ~ quantile(pred_seed_b, na.rm = TRUE, probs = 0),
      play_in == "float2" ~ quantile(pred_seed_b, na.rm = TRUE, probs = 1/3),
      .default = pred_seed_a)) %>%
  select(-.pred_a, -play_in, -.pred_c, -rank_c, -pred_seed_a, -pred_seed_b) %>%
  ungroup()

seed_rmse <- predicted_seeds %>% filter(!is.na(seed)) %>%
  group_by() %>%
  summarise(
    MSE = mean((pred_seed - seed)^2),
    RMSE = sqrt(mean((pred_seed - seed)^2))) %>%
  mutate(Season = "Total")

predicted_seeds %>% filter(!is.na(seed)) %>%
  group_by(season) %>%
  summarise(
    MSE = mean((pred_seed - seed)^2),
    RMSE = sqrt(mean((pred_seed - seed)^2))) %>%
  mutate(Season = as.character(season)) %>%
  select(-season) %>%
  bind_rows(seed_rmse) %>%
  relocate(Season, .before = MSE) %>%
  flextable() %>%
  align(align = "center")

vip(s2_boost_final_fit)

predicted_seeds %>% filter(season == 2024) %>% select(team_name, season, seed, pred_seed) %>%
  filter(seed != pred_seed, seed + 1 != pred_seed, seed - 1 != pred_seed) %>%
  relocate(season, .before = team_name) %>%
  rename(
    "pred seed" = pred_seed,
    "team name" = team_name) %>% 
  rename_with(~str_to_title(.)) %>%
  flextable() %>%
  align(align = "center") %>%
  width(j = 2, width = 1) %>%
  colformat_double(j = c(1,3,4), digits = 0)

# Compare Results ---------------------------------------------------------

simple_seed <- predicted_seeds %>% select(team_name, season, pred_seed) %>%
  full_join(team %>% filter(!is.na(seed)) %>% select(team_name, season, seed), by = join_by(team_name, season)) %>%
  mutate(
    deviance = case_when(
      is.na(pred_seed) ~ 1,
      .default = pred_seed - seed),
    direction = case_when(
      is.na(pred_seed) ~ "over",
      pred_seed > seed ~ "over",
      pred_seed < seed ~ "under",
      .default = "match"))

compare_tourney <- read_csv("Project/full_tourney.csv") %>%
  select(season, winner, contains("team_name")) %>%
  filter(season > 2016) %>%
  left_join(simple_seed %>% rename_with(~ paste0("t1_", .x)), by = join_by(season == t1_season, t1_team_name)) %>%
  left_join(simple_seed %>% rename_with(~ paste0("t2_", .x)), by = join_by(season == t2_season, t2_team_name)) %>%
  mutate(
    play_in = case_when(
      t1_seed == 16 & t2_seed == 16 ~ "play-in",
      t1_seed == 11 & t2_seed == 11 ~ "play-in",
      t1_seed == 12 & t2_seed == 12 ~ "play-in",
      t1_seed == 10 & t2_seed == 10 ~ "play-in",
      .default = NA)) %>%
  rowwise() %>%
  mutate(
    model_result = case_when(
      winner == "t1" & is.na(t1_pred_seed) & !is.na(t2_pred_seed) ~ 0,
      winner == "t1" & !is.na(t1_pred_seed) & is.na(t2_pred_seed) ~ 1,
      winner == "t1" & t1_pred_seed < t2_pred_seed ~ 1,
      winner == "t1" & t2_pred_seed < t1_pred_seed ~ 0,
      winner == "t2" & !is.na(t1_pred_seed) & is.na(t2_pred_seed) ~ 0,
      winner == "t2" & is.na(t1_pred_seed) & !is.na(t2_pred_seed) ~ 1,
      winner == "t2" & t2_pred_seed < t1_pred_seed ~ 1,
      winner == "t2" & t1_pred_seed < t2_pred_seed ~ 0,
      .default = NA),
    model_error = case_when(
      winner == "t1" & is.na(t1_pred_seed) & !is.na(t2_pred_seed) ~ max(12 - t2_pred_seed, 0),
      winner == "t1" & is.na(t2_pred_seed) ~ 0,
      winner == "t1" ~ max(t1_pred_seed - t2_pred_seed, 0),
      winner == "t2" & is.na(t2_pred_seed) & !is.na(t1_pred_seed) ~ max(12 - t1_pred_seed, 0),
      winner == "t2" & is.na(t1_pred_seed) ~ 0,
      winner == "t2" ~ max(t2_pred_seed - t1_pred_seed, 0),
      .default = 0),
    committee_result = case_when(
      winner == "t1" & t1_seed < t2_seed ~ 1,
      winner == "t1" & t2_seed < t1_seed ~ 0,
      winner == "t2" & t2_seed < t1_seed ~ 1,
      winner == "t2" & t1_seed < t2_seed ~ 0,
      .default = NA),
    committee_error = case_when(
      winner == "t1" ~ max(t1_seed - t2_seed, 0),
      winner == "t2" ~ max(t2_seed - t1_seed, 0),
      .default = 0))

compare_tourney %>% select(play_in, contains("team"), winner, contains("seed"),
                           contains("model"), contains("committee")) %>% view()

compare_tourney %>%
  group_by() %>%
  # group_by(season) %>%
  summarise(
    `Model Accuracy` = mean(model_result, na.rm = TRUE),
    `Committee Accuracy` = mean(committee_result, na.rm = TRUE)) %>%
  flextable() %>%
  align(align = "center") %>%
  width(j = c(1,2), width = 1.3) %>%
  colformat_double(digits = 3)

compare_tourney %>%
  filter(is.na(play_in)) %>%
  group_by() %>%
  # group_by(season) %>%
  summarise(
    `Model Mean Error` = mean(model_error),
    `Committee Mean Error` = mean(committee_error)) %>%
  flextable() %>%
  align(align = "center") %>%
  width(j = c(1,2), width = 1.5) %>%
  colformat_double(digits = 3)
  
# Over and Under Seeding --------------------------------------------------

eval_seeding <- predicted_seeds %>% select(team_name, season, pred_seed) %>%
  full_join(team %>% filter(!is.na(seed)) %>% select(team_name, season, seed), by = join_by(team_name, season)) %>%
  left_join(team0 %>% select(season, team_name, conf_abbrev), by = join_by(season, team_name)) %>%
  rowwise() %>%
  mutate(
    deviation = case_when(
      is.na(pred_seed) ~ max(1, 12 - seed),
      is.na(seed) ~ min(-1, pred_seed - 12),
    .default = pred_seed - seed))

# positive is over-seeding, negative is under-seeding
eval_seeding %>% group_by(conf_abbrev) %>%
  summarise(
    Deviation = mean(deviation),
    Sample = n()) %>%
  arrange(desc(Deviation)) %>%
  ungroup() %>%
  rename(
    "Conference" = conf_abbrev) %>%
  dplyr::slice(1:7) %>%
  flextable() %>%
  align(align = "center")

# positive is over-seeding, negative is under-seeding
eval_seeding %>% group_by(conf_abbrev) %>%
  summarise(
    Deviation = mean(deviation),
    Sample = n()) %>%
  arrange(Deviation) %>%
  ungroup() %>%
  rename(
    "Conference" = conf_abbrev) %>%
  dplyr::slice(1:7) %>%
  flextable() %>%
  align(align = "center")

# positive is over-seeding, negative is under-seeding
eval_seeding %>% group_by(team_name) %>%
  summarise(
    Deviation = mean(deviation),
    Sample = n()) %>%
  arrange(desc(Deviation)) %>%
  ungroup() %>%
  rename(
    "Team" = team_name) %>%
  filter(Sample > 3) %>%
  dplyr::slice(1:7) %>%
  flextable() %>%
  align(align = "center") %>%
  width(j = 1, width = 1)
  
eval_seeding %>% group_by(team_name) %>%
  summarise(
    Deviation = mean(deviation),
    Sample = n()) %>%
  arrange(Deviation) %>%
  ungroup() %>%
  rename(
    "Team" = team_name) %>%
  filter(Sample > 3) %>%
  dplyr::slice(1:7) %>%
  flextable() %>%
  align(align = "center") %>%
  width(j = 1, width = 1)
