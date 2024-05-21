# Second Stage Boosting

library("tidymodels")
library("janitor")
library("tidyverse")
library("rvest")
library("xgboost")
library("rpart.plot")
library("vip")

tourney <- read_csv("Project/full_tourney.csv") %>%
  select(-`...1`) %>%
  filter(season > 2016) %>%
  select(-day_num, -contains("_wins"), -contains("_losses"), -num_ot, -contains("conf"), - contains("team_id"),
         -contains("ap"), -contains("ineligible"), -contains("make_tournament"), -contains("score"), -contains("seed"),
         -contains("sag")) %>%
  mutate(index = if_else(season < 2020, season - 2016, season - 2017)) %>%
  na.omit()

boost_tuning_grid <- grid_regular(
  tree_depth(range = c(3, 6)),
  levels = 10)

tourney_total_pred <- tibble()
for(i in 1:6){
  tourney_train <- tourney %>% filter(index != i)
  tourney_test <- tourney %>% filter(index == i)
  tourney_folds <- vfold_cv(tourney_train, v = 5, strata = winner)
  
  class_boost_spec <- boost_tree(trees = 1500, tree_depth = tune()) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
  tourney_recipe <- recipe(winner ~ ., data = tourney_train) %>%
    step_rm(season, t1_team_name, t2_team_name) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())
  
  # fine tune the model
  tourney_boost_tune_results <- tune_grid(
    object = workflow() %>%
      add_recipe(tourney_recipe) %>%
      add_model(class_boost_spec),
    resamples = tourney_folds,
    grid = boost_tuning_grid,
    metrics = metric_set(accuracy))
  
  tourney_boost_best_params <- select_best(tourney_boost_tune_results, "accuracy")
  
  tourney_boost_final_fit <- finalize_workflow(
    workflow() %>%
      add_recipe(tourney_recipe) %>%
      add_model(class_boost_spec),
    tourney_boost_best_params) %>%
    fit(data = tourney_train)
  
  tourney_boost_predictions <- augment(tourney_boost_final_fit, new_data = tourney_test)
  
  tourney_total_pred <- bind_rows(tourney_total_pred, tourney_boost_predictions)
}

tourney_final_predictions <- tourney_total_pred %>%
  arrange(season, desc(.pred_t1)) %>%
  group_by(season)

tourney_final_predictions %>%
  select(contains("team_name"), season, winner, contains("pred")) %>% view()
