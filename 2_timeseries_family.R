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
set.seed(3013)

## family analysis ----
load("data/processed/family_census_little.rda")
# examine value distribution
family_census %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") +
  geom_density() +
  theme_minimal() +
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


family_census %>% 
  filter(scs == 1) %>%  view()
quantile(family_census$population, na.rm = T, probs = c(.01, .98))[2]
skimr::skim(family_census)
family_recipe <- recipe(prop_two_parent ~ ., family_census) %>% 
  step_rm(geo) %>% 
  step_impute_knn(total_0_to_4, two_parent, one_parent, prop_two_parent) %>% 
  step_log(population, population_density, land_area, married, total_private_dwellings, total_0_to_4, one_parent, two_parent) %>% 
  step_normalize(population_change, avg_household_size, avg_family_size) %>% 
  step_zv() %>% 
  prep()

test_family <- bake(family_recipe, new_data = NULL)

  # see how our recipe does in normalizing distributions
test_family %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") +
  geom_density()

tic.clearlog()
tic("Two-Parent Families")
its_model_prop_two_parent1 <- itsa.model(data = as.data.frame(test_family), time = "year", depvar = "prop_two_parent", interrupt_var = "scs", bootstrap = F)

toc(log = TRUE)
time_log <- tic.log(format = F)

load(file = "results/its_prop_two_parent.rda")

postest_family <- itsa.postest(its_model_prop_two_parent)
  # better! Heterogeneousness is improving. Still can't get the plots though
its_model_prop_two_parent[4]
postest_family[4]
