## fitting interrupted time series on census data

## load package(s) 
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

## read in data
load("data/processed/census_1121.rda")

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

## proportion educated below high school 
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


## family analysis ----
  # examine value distribution
family_census %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") +
  geom_density(color = "blue", fill = "red")
  # we have serious skew, so let's remove at outliers at .98
family_census$log10 <- log10(family_census$total_0_to_4)
family_census_an <- subset(family_census, 
                           !(family_census$population > quantile(family_census$population, probs = c(.01, .98), na.rm = T)[2] | family_census$population < quantile(family_census$population, probs = c(.01, .98), na.rm = T)[1]),
                           !(family_census$one_parent > quantile(family_census$one_parent, probs = c(.01, .98), na.rm = T)[2] | family_census$one_parent < quantile(family_census$one_parent, probs = c(.01, .98), na.rm = T)[1]),
                           !(family_census$land_area > quantile(family_census$land_area, probs = c(.01, .98), na.rm = T)[2] | family_census$land_area < quantile(family_census$land_area, probs = c(.01, .98), na.rm = T)[1]),
                           !(family_census$married > quantile(family_census$married, probs = c(.01, .98), na.rm = T)[2] | family_census$married < quantile(family_census$married, probs = c(.01, .98), na.rm = T)[1]),
                           !(family_census$total_0_to_4 > quantile(family_census$total_0_to_4, probs = c(.01, .98), na.rm = T)[2] | family_census$total_0_to_4 < quantile(family_census$total_0_to_4, probs = c(.01, .98), na.rm = T)[1]),
                           !(family_census$total_private_dwellings > quantile(family_census$total_private_dwellings, probs = c(.01, .98), na.rm = T)[2] | family_census$total_private_dwellings < quantile(family_census$total_private_dwellings, probs = c(.01, .98), na.rm = T)[1]),
                           !(family_census$two_parent > quantile(family_census$two_parent, probs = c(.01, .98), na.rm = T)[2] | family_census$two_parent < quantile(family_census$two_parent, probs = c(.01, .98), na.rm = T)[1])
                           )

?lapply
quantile(family_census$population, na.rm = T, probs = c(.01, .98))[2]


family_recipe <- recipe(prop_two_parent ~ ., family_census) %>% 
  step_rm(geo, population, total_total, total_none, married) %>% 
  step_normalize(all_numeric_predictors(), -year, -scs) %>%
  step_impute_knn(all_numeric_predictors()) %>% 
  step_zv() %>% 
  prep()

tic.clearlog()
tic("Two-Parent Families")
its_model_prop_part <- itsa.model(data = as.data.frame(prop_educ_dat_test), time = "year", depvar = "prop_part", interrupt_var = "scs", bootstrap = TRUE, Reps = 100)

toc(log = TRUE)
time_log <- tic.log(format = F)

save(prop_educ_dat_part, its_model_prop_part, file = "results/its_prop_part.rda")


