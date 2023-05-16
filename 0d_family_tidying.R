# Tidying data for children aged 0-4 in intact families

library(tidyverse)

## family data 2011 and 2016
family_1116_og <- read_csv("data/raw/family_status_1116_big.csv") %>% 
  janitor::clean_names()

families_of_interest <- c("Total - Census family structure", "Couple census families with children", "Intact families", "Stepfamilies", "Lone-parent census families")

family_1116_test <- family_1116_og %>% 
  select(9, 3, 4, 7, 12, 19, 24, 28, 34, 38) %>% 
  filter(dim_census_family_structure_including_stepfamily_status_9 %in% families_of_interest)

colnames <- c("year", "geo_level", "geo", "csd_type", "family_structure", "child_0_to_5", "both_0_to_5", "one_0_to_5", "all_0_to_5", "some_0_to_5")
colnames(family_1116_test) <- colnames

family_1116_test <- family_1116_test %>% 
  mutate(
    child_0_to_5 = as.numeric(family_1116_test$child_0_to_5),
    both_0_to_5 = as.numeric(family_1116_test$both_0_to_5),
    one_0_to_5 = as.numeric(family_1116_test$one_0_to_5),
    all_0_to_5 = as.numeric(family_1116_test$all_0_to_5),
    some_0_to_5 = as.numeric(family_1116_test$some_0_to_5)
  ) %>% 
  filter(!is.na(child_0_to_5)) %>% 
  arrange(year)

family_1116_test1 <- family_1116_test %>% 
  group_by(geo, year, family_structure) %>% 
  mutate(
    any_0_to_5 = sum(child_0_to_5, both_0_to_5, one_0_to_5, all_0_to_5, some_0_to_5)
  )

  # remove geos with no census families
no_good <- family_1116_test1 %>% 
  filter(family_structure == "Total - Census family structure") %>% 
  filter(any_0_to_5 == 0) %>% 
  pull(geo)

family_1116 <- family_1116_test1 %>% 
  filter(!(geo %in% no_good))

family_1116

## family data in 2021


