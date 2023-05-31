# Tidying data for children aged 0-4 in intact families

## load packages----
library(tidyverse)
library(here)
library(magrittr)
library(rvest)
library(purrr)
library(pdftools)
library(stringr)

## read in data----
load("data/processed/labor_2011.rda")
labor_2016_og <- read_csv("data/raw/labor_2016_big.csv")
labor_2021_og <- read_csv("data/raw/labor_2021_big.csv")
load("data/processed/census_1121_little.rda")

## tidy 2011 data----
labor_2011 <- labor_2011 %>% 
  select(-employment_rate)

## tidy 2016 data----
labor_2016 <- labor_2016_og %>% 
  janitor::clean_names() %>% 
  filter(dim_age_13a == "Total - Age", dim_sex_3 == "Total - Sex", dim_location_of_study_5 == "Total - Location of study", dim_period_of_immigration_7 == "Total - Period of immigration", dim_admission_category_and_applicant_type_7 == "Total - Admission category and applicant type", dim_highest_certificate_diploma_or_degree_7 != "College, CEGEP or other non-university certificate or diploma") %>% 
  select(1, 4, 26:31, 33, 8)
colnames <- c("year", "geo", "total", "in_labor_force", "employed", "unemployed", "not_in_labor_force", "participation_rate", "unemployment_rate", "educ_level")
colnames(labor_2016) <- colnames

## tidy 2021 data----
labor_2021 <- labor_2021_og %>% 
  janitor::clean_names() %>% 
  filter(indigenous_identity_9 == "Total - Indigenous identity", age_15a == "Total - Age", gender_3 == "Total - Gender", statistics_3 == "Count",
         highest_certificate_diploma_or_degree_16 == "Total - Highest certificate, diploma or degree" | highest_certificate_diploma_or_degree_16 == "No certificate, diploma or degree" | highest_certificate_diploma_or_degree_16 == "High (secondary) school diploma or equivalency certificate" | highest_certificate_diploma_or_degree_16 == "Apprenticeship or trades certificate or diploma" | highest_certificate_diploma_or_degree_16 == "Bachelor’s degree or higher" | highest_certificate_diploma_or_degree_16 == "University certificate or diploma below bachelor level") %>% 
  select(1, 2, 10, 12, 14, 16, 18, 20, 24, 8)
colnames(labor_2021) <- colnames

# polish 2021 geo names
labor_2021$geo <- labor_2021$geo %>% 
  str_remove_all("\\, B\\.C\\.") %>% 
  str_remove_all("\\(CA\\)") %>% 
  str_remove_all("\\(CMA\\)") %>% 
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

## combine labor data----
total_labor <- bind_rows(labor_2011, labor_2016, labor_2021)

total_labor <- total_labor %>% 
  mutate(
    year = factor(year),
    educ_level = factor(educ_level),
    educ_level = fct_collapse(educ_level,
                              `Below High School` = "No certificate, diploma or degree",
                              `High School` = c("Secondary (high) school diploma or equivalency certificate",
                                                "High (secondary) school diploma or equivalency certificate"),
                              Apprenticeship = "Apprenticeship or trades certificate or diploma",
                              Associate = "University certificate or diploma below bachelor level",
                              `Bachelor+` = c("University certificate, diploma or degree at bachelor level or above",
                                              "Bachelor’s degree or higher")
    ))

## calculate education proportions----
total_educ <- total_labor %>% 
  filter(educ_level == "Total - Highest certificate, diploma or degree")
no_educ <- total_labor %>% 
  filter(educ_level == "Below High School")
educ_dat <- bind_cols(total_educ, no_educ[,3:10]) %>%
  mutate(
    total = `total...3`,
    total_uneduc = `total...11`,
    unemployed_total = `unemployed...6`,
    unemployed_uneduc = `unemployed...14`,
    unemployment_rt_total = `unemployment_rate...9`,
    unemployment_rt_uneduc = `unemployment_rate...17`,
    participation_total = `participation_rate...8`,
    participation_uneduc = `participation_rate...16`,
    prop_uneduc = total_uneduc/total,
    prop_part_uneduc = `in_labor_force...12`/`in_labor_force...4`,
    prop_unempl_uneduc = unemployed_uneduc/unemployed_total
  ) %>%
  select(1, 2, 19:29)

## combine with census data
labor_census <- left_join(census_little, educ_dat, by = c("year", "geo"))  
#save(labor_census, file = "data/processed/labor_census_little.rda")


# another issue -- 2016 has messed up labor data compared to census, populations don't add up
# need to fix names with special characters
