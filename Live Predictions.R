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
# Download new Kaggle Data
# Update KPI and SOS excel docs
# check for bugs

team_train <- read_csv(here("Project/MarchMadness_Bracket/Data/full_team.csv")) %>%
  select(-`...1`) %>% filter(ineligible !=1) %>%
  select(-ap, -team_id, -conf_abbrev, -wins, -losses, -nc_wins, -nc_losses, -sag, -ineligible, -conf_classification) %>%
  mutate(make_tournament = factor(make_tournament)) %>%
  filter(season > 2016)

boost_tuning_grid <- grid_regular(
  tree_depth(range = c(3, 6)),
  levels = 10)

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

s1_boost_best_params <- select_best(s1_boost_tune_results, metric = "accuracy")

s1_boost_final_fit <- finalize_workflow(
  workflow() %>%
    add_recipe(s1_recipe) %>%
    add_model(class_boost_spec),
  s1_boost_best_params) %>%
  fit(data = team_train)

s1_boost_predictions <- augment(s1_boost_final_fit, new_data = team_test)

