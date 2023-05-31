## fitting interrupted time series on census data

## load packages----
library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(its.analysis)
library(tictoc)
library(datawizard)

## handle common conflicts
tidymodels_prefer()

## setup parallel processing
registerDoMC(cores = 8)

## seed
set.seed(3013)

## read in data----
load("data/processed/census_1121_little.rda")

## run interrupted time-series model
# restrict data to below bachelor's education
test_participation <- test %>% 
  filter(!is.na(educ_level)) %>% 
  filter(educ_level == "Below High School" | educ_level == "High School" | educ_level == "Apprenticeship")

tic.clearlog()
tic("Participation Rates")
its_model_part <- itsa.model(data = as.data.frame(test_participation), time = "year", depvar = "participation_rate", interrupt_var = "scs", bootstrap = FALSE)
save(its_model_part, file = "results/its_participation.rda")

toc(log = TRUE)
time_log <- tic.log(format = F)

res_participation <- itsa.postest(its_model_part, no.plots = FALSE, bootstrap = FALSE, print = T)
save(res_participation, time_log, file = "results/results_part.rda")

## proportion educated below high school level
load("data/processed/prop_educ_dat.rda")
recipe <- recipe(prop_educ ~ ., prop_educ_dat) %>% 
  step_rm(geo, population, total_total, total_none, married) %>% 
  step_normalize(all_numeric_predictors(), -year, -scs) %>%
  step_impute_knn(all_numeric_predictors()) %>% 
  step_zv() %>% 
  prep()
# take 2
recipe <- recipe(prop_educ ~ ., prop_educ_dat) %>% 
  step_rm(geo) %>% 
  step_log(population, total_total, total_none, married) %>% 
  step_impute_knn(all_numeric_predictors()) %>% 
  step_zv() %>% 
  prep()
class(prop_educ_dat$population)

prop_educ_dat_part <- bake(recipe, new_data = NULL)

tic.clearlog()
tic("Proportion Educated Below High School Participating in Labor Force")
its_model_prop_part <- itsa.model(data = as.data.frame(prop_educ_dat_part), time = "year", depvar = "prop_part", interrupt_var = "scs", bootstrap = TRUE, Reps = 100)

toc(log = TRUE)
time_log <- tic.log(format = F)

save(prop_educ_dat_part, its_model_prop_part, file = "results/its_prop_part.rda")


res_prop_part <- itsa.postest(its_model_prop_part, no.plots = FALSE, bootstrap = FALSE, print = T)
save(res_prop_educ, time_log, file = "results/results_prop_educ.rda")
res_prop_part

## proportion participating in labor force educated below high school 
prop_educ_dat_part <- bake(recipe, new_data = NULL)

tic.clearlog()
tic("Proportion Educated Below High School Participating in Labor Force")
its_model_prop_part <- itsa.model(data = as.data.frame(prop_educ_dat_test), time = "year", depvar = "prop_part", interrupt_var = "scs", bootstrap = TRUE, Reps = 100)

toc(log = TRUE)
time_log <- tic.log(format = F)

save(prop_educ_dat_part, its_model_prop_part, file = "results/its_prop_part.rda")


res_prop_part <- itsa.postest(its_model_prop_part, no.plots = FALSE, bootstrap = FALSE, print = T)
save(res_prop_educ, time_log, file = "results/results_prop_educ.rda")
res_prop_part

ggplot(prop_educ_dat_test) +
  geom_histogram(aes(scs))

res_participation[[1]]
its_model_prop_part[[7]]
its_model_prop_educ[[7]]
its_model[[3]]

