# Future Predictions
# Date: February 27, 2025
# This script takes the model from the report and applies it to data in future seasons

library("tidymodels")
library("janitor")
library("rvest")
library("xgboost")
library("rpart.plot")
library("vip")
library("here")
library("tidyverse"); theme_set(theme_minimal())

# Data Update Process Each Year
# 1. Download new Kaggle Data (https://www.kaggle.com/competitions/march-machine-learning-mania-2025/data)
# 2. Download KPI (https://faktorsports.com/#/home)
# 3. SOS should scrape by itself
# 4. check conference champions (https://www.espn.com/espn/feature/story/_/page/bracketology/ncaa-bracketology-2025-march-madness-men-field-predictions)
# check for bugs

# load data
team_data <- read_csv(here("Project/MarchMadness_Bracket/Data/full_team.csv"), show_col_types = FALSE) %>%
  select(-`...1`) %>% filter(ineligible !=1) %>%
  select(-ap, -team_id, -conf_abbrev, -wins, -losses, -nc_wins, -nc_losses, -sag, -ineligible, -conf_classification) %>%
  mutate(make_tournament = factor(make_tournament)) %>%
  filter(season > 2016)

team_train <- team_data %>% filter(season != 2025)
team_test <- team_data %>% filter(season == 2025)

boost_tuning_grid <- grid_regular(
  tree_depth(range = c(3, 6)),
  levels = 10)

set.seed(1128)
team_folds <- vfold_cv(team_train, v = 5, strata = make_tournament)

# predict make tournament
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

s1_boost_best_params <- select_best(s1_boost_tune_results, metric = "accuracy")

s1_boost_final_fit <- finalize_workflow(
  workflow() %>%
    add_recipe(s1_recipe) %>%
    add_model(class_boost_spec),
  s1_boost_best_params) %>%
  fit(data = team_train)

s1_boost_predictions <- augment(s1_boost_final_fit, new_data = team_test)

make_tournament_pred <- s1_boost_predictions %>%
  arrange(season, desc(.pred_1)) %>%
  group_by(season) %>%
  mutate(
    pred_make_tournament = if_else(rank(.pred_0) < 69, 1, 0)) %>%
  select(-.pred_1, -.pred_0, -.pred_class)

# predict seed in tournament
order_train <- team_train %>% filter(make_tournament == 1) %>% select(-contains("tournament"))
order_test <- make_tournament_pred %>% filter(pred_make_tournament == 1) %>% select(-contains("tournament"))
set.seed(1128)
order_folds <- vfold_cv(order_train, v = 5, strata = seed)

reg_boost_spec <- boost_tree(trees = 1500, tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

s2_recipe <- recipe(seed ~ ., data = order_train) %>%
  step_rm(season, team_name) %>%
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

predicted_seeds <- s2_boost_predictions %>% 
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

# Results -----------------------------------------------------------------

# bubble
s1_boost_predictions %>%
  arrange(desc(.pred_1)) %>%
  dplyr::slice(61:80) %>%
  rename(make_tournament_prob = .pred_1) %>%
  select(team_name,make_tournament_prob) %>%
  mutate(
    rank = rank(-make_tournament_prob),
    category = case_when(
      rank < 5 ~ "First Four Byes",
      rank < 9 ~ "Last Four In",
      rank < 13 ~ "First Four Out",
      rank < 17 ~ "Next Four Out",
      rank < 21 ~ "Contention"))

# predicted seeds
predicted_seeds %>% select(team_name, pred_seed) %>%
  print(n=68)

