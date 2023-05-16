# I should just put this in 00_exploration.R

library(tidyverse)
library(tidymodels)
library(themis)
library(poissonreg)


census <- readRDS("data/processed/final_census.rds") %>% 
  mutate(
    scs = factor(scs),
    year = factor(year)
  )
labor <- readRDS("data/processed/final_labor.rds")
crime <- readRDS("data/processed/final_crime.rds")
scs <- readRDS("data/processed/final_scs.rds")

# splitting and folding data
census_split <- initial_split(census, .8, strata = scs)

census_train <- training(census_split)
census_test <- testing(census_split)
census_folds <- vfold_cv(census_train, v = 10, repeats = 3, strata = scs)

# Establish recipe
recipe <- recipe(scs ~ ., data = census_train) %>% 
  step_rm(med_income, income_lower_half, gini, population_last, geo) %>% 
  step_impute_mean(population_current, total_private_dwellings, population_density, avg_age, avg_household_size, married, avg_family_size, unemployment) %>% 
  step_pca() %>% 
  step_normalize(starts_with("population"), starts_with("avg"), land_area, unemployment) %>% 
  step_upsample(scs) %>% 
  prep()

skimr::skim(census)

bake(recipe, new_data = NULL)

# Establish models
rf_model <- rand_forest(mode = "regression",
                        min_n = tune(),
                        mtry = tune()) %>% 
  set_engine("ranger")

tree_model <- boost_tree(mode = "regression",
                         min_n = tune(),
                         mtry = tune(),
                         learn_rate = tune()) %>% 
  set_engine("xgboost")

knn_model <- nearest_neighbor(mode = "regression",
                              neighbors = tune()) %>% 
  set_engine("kknn")

# Set hyperparameter tuning
rf_params <- extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(c(1, 13))) 
rf_grid <- grid_regular(rf_params, levels = 5)

tree_params <- extract_parameter_set_dials(tree_model) %>% 
  update(mtry = mtry(c(1, 13))) 
tree_grid <- grid_regular(tree_params, levels = 5)

knn_params <- extract_parameter_set_dials(knn_model)
knn_grid <- grid_regular(knn_params, levels = 5)  

# Make workflows
rf_wkflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe)

tree_wkflow <- workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(recipe)

knn_wkflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(recipe)

#Optimize tunings
rf_tuned <- rf_wkflow %>% 
  tune_grid(census_folds, grid = rf_grid)

saveRDS(rf_tuned, file = "fitted/rf_tuned.rds")

tree_tuned <- tree_wkflow %>% 
  tune_grid(census_folds, grid = tree_grid)

saveRDS(tree_tuned, file = "fitted/tree_tuned.rds")

knn_tuned <- knn_wkflow %>% 
  tune_grid(census_folds, grid = knn_grid)

saveRDS(knn_tuned, file = "fitted/knn_tuned.rds")

autoplot(rf_tuned, metric = "rmse")
autoplot(knn_tuned, metric = "rmse")

# Fitting winning model
select_best(rf_tuned, metric = "rmse")
select_best(knn_tuned, metric = "rmse")

show_best(rf_tuned) %>% select(-.estimator)
show_best(knn_tuned) %>% select(-.estimator)

rf_wkflow_tuned <- rf_wkflow %>% 
  finalize_workflow(select_best(rf_tuned, metric = "rmse"))

rf_results <- fit(rf_wkflow_tuned, census_train)

census_metric <- metric_set(rmse)

rf_pred <- predict(rf_results, new_data = census_test) %>% 
  bind_cols(census_test %>% select(scs))

rf_pred %>% 
  census_metric(truth = scs, estimate = .pred)

ggplot(rf_pred) +
  geom_point(aes(.pred, scs), size = 2, alpha = .7) +
  geom_abline(color = "red", lwd = 2, alpha = .5) +
  labs(
    x = "Predicted",
    y = "Actual"
  )


log_lin_spec <- poisson_reg()

log_lin_fit <- 
  log_lin_spec %>% 
  fit(scs ~ ., data = census_train)
log_lin_fit

metrics <- collect_metrics(rf_tuned) %>% 
  filter(mean == max(mean))

view(metrics)