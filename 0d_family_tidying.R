# Tidying data for children aged 0-4 in intact families

library(tidyverse)
library(tictoc)

## family data in 2011 and 2016
family_16_og <- read_csv("data/raw/family_1116_little.csv") %>% 
  janitor::clean_names()

families_of_interest <- c("Total - Census family structure", "Couple census families with children", "Intact families", "Stepfamilies", "Lone-parent census families")

family_16_test <- family_16_og %>% 
  filter(dim_sex_3 == "Total - Sex", dim_age_of_child_13 == "0 to 4 years") %>% 
  select(8, 4, 17, 18, 21)
colnames <- c("year", "geo", "total", "two_parent", "one_parent")
colnames(family_16_test) <- colnames

## family data in 2021
family_21_og <- read_csv("data/raw/family_21_big.csv") %>% 
  janitor::clean_names()

families_of_interest_21 <- c("Total - Children in census families", pull(family_21_og[3,7]), "Living in a one-parent family")

family_21_test <- family_21_og %>% 
  filter(age_group_10 == "0 to 4 years", gender_3a == "Total - Gender",
         household_and_family_characteristics_of_persons_including_detailed_information_on_stepfamilies_25 %in% families_of_interest_21) %>% 
  select(4, 2, 7, 9:26) %>% 
  select(-starts_with("symbol")) %>% 
  select(1:4)

colnames(family_21_test) <- c("year", "geo", "family_structure", "total")

family_21_test1 <- family_21_test %>% 
  pivot_wider(names_from = family_structure, values_from = total) %>% 
  filter(year == 2021)

colnames(family_21_test1) <- colnames

## combine family data
family_total <- bind_rows(family_21_test1, family_16_test) %>% 
  arrange(year) %>% 
  mutate(
    prop_two_parent = two_parent/total,
    year = factor(year)
  )
family_total

distinct(census, geo)
distinct(family_total, geo)

family_census <- left_join(family_total, census, by = c("geo", "year"))


# run time series

library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(its.analysis)
library(tictoc)

## handle common conflicts
tidymodels_prefer()

## setup parallel processing
registerDoMC(cores = 8)

skimr::skim(family_census)
  # I'm fucked
recipe <- recipe(prop_two_parent ~ ., family_census) %>% 
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
