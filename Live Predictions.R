# Future Predictions
# Date: March 2, 2025
# This script takes the model from the report and applies it to data in future seasons

library("tidymodels")
library("janitor")
library("xgboost")
library("rpart.plot")
library("vip")
library("here")
library("tidyverse"); theme_set(theme_minimal())
library("tictoc")
library("furrr")


# Directions --------------------------------------------------------------

# Data Update Process Each Year - in Project Tidy.R
# 1. Download new Kaggle Data (https://www.kaggle.com/competitions/march-machine-learning-mania-2025/data)
# 2. Download KPI (https://faktorsports.com/#/home)
# 3. SOS should scrape by itself
# 4. check conference champions (https://www.espn.com/espn/feature/story/_/page/bracketology/ncaa-bracketology-2025-march-madness-men-field-predictions)
# 5. rerun Project Tidy.R
# source(here("Project/MarchMadness_Bracket/Project Tidy.R"))
# check for bugs

# Setting Up --------------------------------------------------------------

# load data
team_data <- read_csv(here("Project/MarchMadness_Bracket/Data/full_team.csv"), show_col_types = FALSE) %>%
  select(-`...1`) %>% filter(ineligible !=1) %>%
  select(-ap, -team_id, -conf_abbrev, -wins, -losses, -nc_wins, -nc_losses, -sag, -ineligible, -conf_classification) %>%
  mutate(make_tournament = factor(make_tournament)) %>%
  filter(season > 2016)

# get data ready
team_train <- team_data %>% filter(season != 2025)
team_test <- team_data %>% filter(season == 2025)

# tuning grid
boost_tuning_grid <- grid_regular(
  tree_depth(range = c(3, 6)),
  levels = 10)

# set model
class_boost_spec <- boost_tree(trees = 1000, tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

reg_boost_spec <- boost_tree(trees = 1000, tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Functions ---------------------------------------------------------------

# predict make tournament
predict_inclusion <- function(training_data, testing_data, bubble = TRUE){
  # remove automatic bids
  train_data <- training_data %>% filter(conf_result != "champion")
  auto_bids <- testing_data %>% filter(conf_result == "champion") %>% nrow()
  test_data <- testing_data %>% filter(conf_result != "champion")
  
  team_folds <- vfold_cv(train_data, v = 5, strata = make_tournament)
  
  # recipe
  s1_recipe <- recipe(make_tournament ~ ., data = train_data) %>%
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
  
  s1_boost_best_params <- select_best(s1_boost_tune_results, metric = "accuracy")
  
  s1_boost_final_fit <- finalize_workflow(
    workflow() %>%
      add_recipe(s1_recipe) %>%
      add_model(class_boost_spec),
    s1_boost_best_params) %>%
    fit(data = train_data)
  
  results <- augment(s1_boost_final_fit, new_data = test_data) %>%
    arrange(desc(.pred_1))
  
  if(bubble == TRUE){
    list(
      bind_rows(testing_data %>% filter(conf_result == "champion"),
                results %>% dplyr::slice(1:(68-auto_bids))) %>%
        select(team_name),
      results %>% dplyr::slice((69-auto_bids):(80-auto_bids)) %>%
        select(team_name)) %>% return()
  }else{
    results %>% dplyr::slice(1:(68-auto_bids)) %>%
      bind_rows(testing_data %>% filter(conf_result == "champion")) %>%
      select(team_name) %>% return()
    }
}

# predict seeds
predict_seed <- function(train_included, test_included){
  
  folds_included <- vfold_cv(train_included, v = 5, strata = seed)
  
  s2_recipe <- recipe(seed ~ ., data = train_included) %>%
    step_rm(season, team_name) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())
  
  # fine tune the model
  s2_boost_tune_results <- tune_grid(
    object = workflow() %>%
      add_recipe(s2_recipe) %>%
      add_model(reg_boost_spec),
    resamples = folds_included,
    grid = boost_tuning_grid,
    metrics = metric_set(rmse))
  
  s2_boost_best_params <- select_best(s2_boost_tune_results, metric = "rmse")
  
  s2_boost_final_fit <- finalize_workflow(
    workflow() %>%
      add_recipe(s2_recipe) %>%
      add_model(reg_boost_spec),
    s2_boost_best_params) %>%
    fit(data = train_included)
  
  augment(s2_boost_final_fit, new_data = test_included) %>%
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
    ungroup() %>%
    select(team_name, pred_seed)
}

# Bootstrapping for UQ ~danger... this takes 31+ minutes
tic()
plan(multisession, workers = 4) # use multiple R sessions
train_splits <- bootstraps(team_train, times = 500)$splits #bootstrap training data for UQ

uq <- future_map_dfr(train_splits, ~{
  # predict teams that are included in tournament
  pred_inclusion <- predict_inclusion(training_data = analysis(.x),
                                      testing_data = team_test, bubble = FALSE)
  
  # predict seeding of included teams
  pred_seeds <- predict_seed(train_included = analysis(.x) %>% filter(make_tournament == 1), #bootstrapped training data
                        test_included = pred_inclusion %>% left_join(team_test, by = join_by(team_name))) #included data w info
  gc() # recover lost memory
  
  team_test %>% select(team_name) %>%
    left_join(pred_seeds, by = join_by(team_name)) %>%
    as.matrix() %>%
    t() %>%
    as.data.frame() %>%
    as_tibble() %>%
    row_to_names(1)
},
.options = furrr_options(seed = TRUE),
.progress = TRUE)

future:::ClusterRegistry("stop")
toc()

# find summary stats of bootstrap
boot_summary <- uq %>% as.matrix() %>% t() %>%
  as.data.frame() %>%
lapply(function(x) as.numeric(as.character(x))) %>%
  bind_cols() %>%
  mutate(team_name = colnames(uq)) %>%
  relocate(team_name) %>%
  mutate(
    seeds = rowMeans(across(contains("V")), na.rm = TRUE),
    across(contains("V"), ~if_else(is.na(.), 0, 1)),
    inclusion_prob = rowMeans(across(contains("V")))) %>%
  select(team_name, seeds, inclusion_prob) %>%
  arrange(desc(inclusion_prob), seeds)

# Exact Results -----------------------------------------------------------------

# predict inclusion of at large teams ~ 9 seconds
included_teams <- predict_inclusion(team_train, team_test, bubble = FALSE) %>%
  left_join(team_test, by = join_by(team_name))

# predict seed ~3.6 seconds
team_train %>% filter(make_tournament == 1) %>% predict_seed(included_teams) %>% print(n=68)

# bubble
predict_inclusion(team_train, team_test, bubble = TRUE) %>%
  bind_rows() %>%
  dplyr::slice(61:80) %>%
  select(team_name) %>%
  mutate(
    index = row_number(),
    category = case_when(
      index < 5 ~ "First Four Byes",
      index < 9 ~ "Last Four In",
      index < 13 ~ "First Four Out",
      index < 17 ~ "Next Four Out",
      index < 21 ~ "Contention")) %>%
  select(-index)

# UQ
boot_summary %>% print(n=12)

bubble <- c("Texas", "Xavier", "San Diego St", "North Carolina", "West Virginia", "Indiana", "Ohio St", "Boise St")

# UQ bubble
boot_summary %>%
  filter(team_name %in% bubble) %>%
  mutate(team_name = factor(team_name, levels = bubble)) %>%
  arrange(team_name)

boot_summary %>%
  filter(inclusion_prob > 0, inclusion_prob < 1)

boot_summary %>%
  mutate(index = row_number()) %>%
  relocate(index) %>%
  dplyr::slice(64:77)

# write_csv(boot_summary, here("Project/MarchMadness_Bracket/boot_predictions25.csv"))
boot_summary <- read_csv(here("Project/MarchMadness_Bracket/boot_predictions25.csv"))

# conference disparity
boot_summary %>% 
  left_join(read_csv(here("Project/MarchMadness_Bracket/Data/full_team.csv"), show_col_types = FALSE) %>% filter(season == 2025), by = join_by(team_name)) %>%
  select(team_name, seeds, inclusion_prob, seed, conf_abbrev) %>%
  rename(predicted_seed = seeds) %>%
  mutate(
    seed_diff = case_when(!is.na(seed) & !is.na(predicted_seed) ~ predicted_seed - seed,
                          !is.na(seed) & is.na(predicted_seed) ~ 12 - seed,
                          is.na(seed) & !is.na(predicted_seed) ~ predicted_seed - 12,
                          .default = NA)) %>% # positive is overseeded, negative is underseeded
  group_by(conf_abbrev) %>%
  summarize(
    average_disparity = mean(seed_diff, na.rm = TRUE),
    teams = sum(!is.na(seed_diff))) %>%
  arrange(desc(average_disparity))

# individual disparity
boot_summary %>% 
  left_join(read_csv(here("Project/MarchMadness_Bracket/Data/full_team.csv"), show_col_types = FALSE) %>% filter(season == 2025), by = join_by(team_name)) %>%
  select(team_name, seeds, inclusion_prob, seed, conf_abbrev) %>%
  rename(predicted_seed = seeds) %>%
  mutate(
    seed_diff = case_when(!is.na(seed) & !is.na(predicted_seed) ~ predicted_seed - seed,
                          !is.na(seed) & is.na(predicted_seed) ~ 12 - seed,
                          is.na(seed) & !is.na(predicted_seed) ~ predicted_seed - 12,
                          .default = NA)) %>% # positive is overseeded, negative is underseeded
  arrange(desc(abs(seed_diff)))

