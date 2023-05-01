## fitting interrupted time series on census data

## load package(s) 
library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(its.analysis)

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
  filter(educ_level != "Below High School" | educ_level == "High School" | educ_level == "Apprenticeship")
its_model_part <- itsa.model(data = as.data.frame(test_participation), time = "year", depvar = "participation_rate", interrupt_var = "scs", bootstrap = FALSE)
save(its_model_part, file = "results/its_participation.rda")

census_2011 %>% 
  distinct(geo)
geo_2016 <- census_2016 %>% 
  distinct(geo) %>% 
  pull()
c2011 <- census_2011 %>% 
  filter(geo %in% geo_2016)
census_2021 %>% 
  distinct(geo)
