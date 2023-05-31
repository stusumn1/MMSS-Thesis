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
load("data/processed/census_1121.rda")
load("data/processed/census_1121_little.rda")
load("data/processed/labor_census_little.rda")

## run interrupted time-series model
  # restrict data to below bachelor's education
test_participation <- test %>% 
  filter(!is.na(educ_level)) %>% 
  filter(educ_level == "Below High School") %>% 
  select(-2, -15, -17)

tic.clearlog()
tic("Participation Rates")

its_model_part <- itsa.model(data = as.data.frame(test_participation), 
                             time = "year", 
                             depvar = "participation_rate", 
                             interrupt_var = "scs", 
                             Reps = 100)

toc(log = TRUE)
time_log <- tic.log(format = F)

part_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

res_participation <- itsa.postest(its_model_part)

save(its_model_part, file = "results/its_participation.rda")
save(res_participation, time_log, file = "results/results_part.rda")

## proportion educated below high school level
load("data/processed/labor_census_little.rda")
labor_census <- labor_census %>% 
  filter(!is.na(prop_part_uneduc))

# examine value distribution
part_uneduc_dat %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") +
  geom_density() +
  theme_minimal() +
  scale_x_continuous(breaks = NULL)

recipe <- recipe(prop_part_uneduc ~ ., labor_census) %>% 
  step_rm(geo, land_area, total, total_uneduc, unemployed_total, unemployed_uneduc, participation_total, participation_uneduc) %>% 
  step_impute_knn(all_numeric_predictors()) %>% 
  step_log(married, population, population_density, total_private_dwellings, unemployment_rt_total, unemployment_rt_uneduc) %>% 
  step_normalize(avg_age, avg_family_size, avg_household_size, population_change) %>%
  step_nzv() %>% 
  prep()


## proportion participating in labor force educated below high school 
part_uneduc_dat <- bake(recipe, new_data = NULL)

tic.clearlog()
tic("Proportion Educated Below High School Participating in Labor Force")

uneduc_part_mod <- itsa.model(data = as.data.frame(part_uneduc_dat), 
                              time = "year", 
                              depvar = "prop_part_uneduc", 
                              interrupt_var = "scs", 
                              covariates = "population_change", 
                              Reps = 200)

toc(log = TRUE)
time_log <- tic.log(format = F)

uneduc_part_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

uneduc_part_res <- itsa.postest(uneduc_part_mod)
uneduc_part_res[[2]]

save(uneduc_part_tictoc, uneduc_part_mod, uneduc_part_res, file = "results/prop_part_uneduc.rda")

  # -.035%, p = .064. Should add covariates of prop_uneduc
  # added covariates prop_uneduc, resulted in p-value .3
  # added covariate population_change, worked better
    # p = .076

ggplot(part_uneduc_dat) +
  geom_point(aes(prop_uneduc, prop_part_uneduc, size = factor(scs), color = factor(scs)), alpha = .7) +
  theme_minimal()
