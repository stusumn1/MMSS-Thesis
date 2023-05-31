# Tidying data for children aged 0-4 in intact families

## load packages----
library(tidyverse)

## tidy 2011 and 2016 family data----
family_16_og <- read_csv("data/raw/family_1116_little.csv") %>% 
  janitor::clean_names()

family_16_test <- family_16_og %>% 
  filter(dim_sex_3 == "Total - Sex", dim_age_of_child_13 == "0 to 4 years") %>% 
  select(8, 4, 17, 18, 21)
colnames <- c("year", "geo", "total_0_to_4", "two_parent", "one_parent")
colnames(family_16_test) <- colnames

## tidy 2021 family data----
family_21_og <- read_csv("data/raw/family_21_big.csv") %>% 
  janitor::clean_names()
  
  # define family details of interest
families_of_interest_21 <- c("Total - Children in census families", pull(family_21_og[3,7]), "Living in a one-parent family")
  
  # restrict data to family details of interest and children aged 0-4
family_21_test <- family_21_og %>% 
  filter(age_group_10 == "0 to 4 years", gender_3a == "Total - Gender",
         household_and_family_characteristics_of_persons_including_detailed_information_on_stepfamilies_25 %in% families_of_interest_21) %>% 
  select(4, 2, 7, 9:26) %>% 
  select(-starts_with("symbol")) %>% 
  select(1:4)

colnames(family_21_test) <- c("year", "geo", "family_structure", "total")

family_21 <- family_21_test %>% 
  pivot_wider(names_from = family_structure, values_from = total) %>% 
  filter(year == 2021)

colnames(family_21) <- colnames

  # polish 2021 geo names
family_21$geo <- family_21$geo %>% 
  str_remove_all("\\(.*\\)") %>% 
  str_remove_all(" , Ont\\.") %>% 
  str_remove_all(" , Que\\.") %>% 
  str_remove_all(" , N\\.B\\.") %>% 
  str_remove_all(" , N\\.L\\.") %>% 
  str_remove_all(" , N\\.S\\.") %>% 
  str_remove_all(" , P\\.E\\.I\\.") %>% 
  str_remove_all(" , N\\.B\\.\\/Que") %>% 
  str_remove_all(" , B\\.C\\.") %>% 
  str_remove_all(" , Alta\\.") %>% 
  str_remove_all(" , Sask\\.") %>% 
  str_remove_all(" , Man\\.") %>% 
  str_remove_all(" , Ont\\.") %>% 
  str_remove_all(" , Ont\\.\\/Que\\.") %>% 
  str_remove_all(" , Y\\.T\\.") %>% 
  str_remove_all(" , N\\.W\\.T\\.")

## combine family data----
family_total <- bind_rows(family_21, family_16_test) %>% 
  arrange(year) %>% 
  mutate(
    prop_two_parent = two_parent/total_0_to_4,
    year = factor(year)
  )
family_total

## incorporate into larger census data----
family_census <- left_join(census_little, family_total, by = c("geo", "year"))
#save(family_census, file = "data/processed/family_census_little.rda")
family_census %>% 
  filter(is.na(prop_two_parent))
# need to adjust the names for special characters
