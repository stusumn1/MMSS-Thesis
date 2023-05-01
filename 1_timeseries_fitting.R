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
its_model <- itsa.model(data = as.data.frame(census), time = "year", depvar = "married", interrupt_var = "scs", bootstrap = TRUE, Reps = 250)
census_2011 %>% 
  distinct(geo)
geo_2016 <- census_2016 %>% 
  distinct(geo) %>% 
  pull()
c2011 <- census_2011 %>% 
  filter(geo %in% geo_2016)
census_2021 %>% 
  distinct(geo)
