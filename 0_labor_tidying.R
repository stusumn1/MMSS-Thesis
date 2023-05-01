## combining labor data

## load packages
library(tidyverse)
library(here)
library(magrittr)
library(rvest)
library(purrr)
library(pdftools)
library(stringr)

## read in data
load("data/processed/labor_2011.rda")
labor_2016_og <- read_csv("data/raw/labor_2016_big.csv")
labor_2021_og <- read_csv("data/raw/labor_2021_big.csv")

## tidying 2016 data
labor_2016 <- labor_2016_og %>% 
  janitor::clean_names() %>% 
  filter(dim_age_13a == "Total - Age", dim_sex_3 == "Total - Sex", dim_location_of_study_5 == "Total - Location of study", dim_period_of_immigration_7 == "Total - Period of immigration", dim_admission_category_and_applicant_type_7 == "Total - Admission category and applicant type", dim_highest_certificate_diploma_or_degree_7 != "College, CEGEP or other non-university certificate or diploma") %>% 
  select(1, 4, 26:31, 33, 8)
colnames <- c("year", "geo_name", "total", "in_labor_force", "employed", "unemployed", "not_in_labor_force", "participation_rate", "unemployment_rate", "educ_level")
colnames(labor_2016) <- colnames

## tidying 2021 data
labor_2021 <- labor_2021_og %>% 
  janitor::clean_names() %>% 
  filter(indigenous_identity_9 == "Total - Indigenous identity", age_15a == "Total - Age", gender_3 == "Total - Gender", statistics_3 == "Count",
        highest_certificate_diploma_or_degree_16 == "Total - Highest certificate, diploma or degree" | highest_certificate_diploma_or_degree_16 == "No certificate, diploma or degree" | highest_certificate_diploma_or_degree_16 == "High (secondary) school diploma or equivalency certificate" | highest_certificate_diploma_or_degree_16 == "Apprenticeship or trades certificate or diploma" | highest_certificate_diploma_or_degree_16 == "Bachelor’s degree or higher" | highest_certificate_diploma_or_degree_16 == "University certificate or diploma below bachelor level") %>% 
  select(1, 2, 10, 12, 14, 16, 18, 20, 24, 8)
colnames(labor_2021) <- colnames
labor_2021 %>% 
  view()

labor_2021$geo_name <- labor_2021$geo_name %>% 
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
  
## combing labor data
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

## combine labor and census data
load("data/processed/census_1121.rda")
census <- census %>% 
  mutate(
    year = factor(year)
  )
test <- full_join(census, total_labor, by = c("year", "geo" = "geo_name"))

## add presence of scs site
load("data/processed/final_scs.rda")
#total_labor$scs <- 0
y2011 <- total_labor %>% 
  filter(year == "2011")
y2016 <- total_labor %>% 
  filter(year == "2016")
y2021 <- total_labor %>% 
  filter(year == "2021")

scs_city <- distinct(final_scs, city) %>% 
  pull()

y2021_test <- y2021 %>% 
  mutate(
    scs = ifelse(
      geo_name %in% scs_city, 1, 0
    )
  )
y2021_test %>% 
  filter(scs == 1)
y2011 %>% 
  filter(geo_name == "Calgary")
census11_names <- labor_2011 %>% 
  filter(year == 2011) %>% 
  distinct(geo_name) %>% 
  pull()
test_labor <- total_labor %>% 
  filter(geo_name %in% census11_names)
census <- census %>% 
  mutate(
  year = factor(year)
)


census %>% 
  filter(scs == 1)
