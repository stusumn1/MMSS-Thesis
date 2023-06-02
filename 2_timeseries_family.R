## fitting interrupted time series on family census data

## load packages----
library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(its.analysis)
library(tictoc)
library(datawizard)
library(scales)

## handle common conflicts
tidymodels_prefer()

## setup parallel processing
registerDoMC(cores = 8)

## seed
set.seed(4444)

## family analysis ----
load("data/processed/family_census_little.rda")
family_census <- family_census %>% 
  filter(!is.na(prop_two_parent))

# examine value distribution
family_census %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") +
  geom_density() +
  theme_bw() +
  scale_x_continuous(breaks = NULL)

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
) #need to work through this more

# feature engineering/recipe
family_recipe <- recipe(prop_two_parent ~ ., family_census) %>% 
  step_rm(geo, population, land_area, avg_family_size, two_parent, one_parent) %>% 
  step_filter_missing(all_predictors(), threshold = .2) %>% 
  step_impute_knn(total_0_to_4, prop_two_parent) %>% 
  step_log(population_density, married, total_private_dwellings, total_0_to_4) %>% 
  step_normalize(population_change, avg_household_size) %>% 
  step_zv() %>% 
  prep()

## proportion in two-parent families aged 0-4
family <- bake(family_recipe, new_data = NULL) %>% 
  mutate(scs = factor(scs))

  # see how our recipe does in normalizing distributions
family %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") +
  geom_density() +
  theme_bw() +
  scale_x_continuous(breaks = NULL)


## proportion in two-parent families aged 0-4
tic.clearlog()
tic("Two-Parent Families")
family_model <- itsa.model(data = as.data.frame(family), 
                               time = "year", 
                               depvar = "prop_two_parent", 
                               interrupt_var = "scs", 
                               Reps = 200)

toc(log = TRUE)
time_log <- tic.log(format = F)
family_tictoc <- tibble::tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

save(family, family_model, family_tictoc, file = "results/family_res.rda")
family_postest <- itsa.postest(family_model)
  # better! Heterogeneity is improving. Still can't get the plots though
its_model_prop_two_parent[2]
postest_family[4]
