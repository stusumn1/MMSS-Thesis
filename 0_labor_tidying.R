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
labor_2016_og <- read_csv("data/raw/labor_2016.csv")
labor_2021_og <- read_csv("data/raw/labor_2021.csv")

## tidying 2016 data
labor_2016 <- labor_2016_og %>% 
  janitor::clean_names() %>% 
  filter(dim_age_13a == "Total - Age", dim_sex_3 == "Total - Sex", dim_location_of_study_5 == "Total - Location of study", dim_period_of_immigration_7 == "Total - Period of immigration") %>% 
  select(1, 4, 26:31, 33, 8)

colnames <- c("year", "geo_name", "total", "in_labor_force", "employed", "unemployed", "not_in_labor_force", "participation_rate", "unemployment_rate", "educ_level")
colnames(labor_2016) <- colnames

## tidying 2021 data
labor_2021 <- labor_2021_og %>% 
  janitor::clean_names() %>% 
  filter(indigenous_identity_9 == "Total - Indigenous identity", age_15a == "Total - Age", gender_3 == "Total - Gender", statistics_3 == "Count") %>% 
  select(1, 2, 10, 12, 14, 16, 18, 20, 24, 8)
colnames(labor_2021) <- colnames

total_labor <- bind_rows(labor_2011, labor_2016, labor_2021)

total_labor <- total_labor %>% 
  mutate(
    year = factor(year),
    educ_level = factor(educ_level),
    educ_level = fct_collapse(educ_level,
                             `Below High School` = "No certificate, diploma or degree",
                             `High School` = c("Secondary (high) school diploma or equivalency certificate",
                                               "High (secondary) school diploma or equivalency certificate"),
                             Apprenticeship = c("Apprenticeship or trades certificate or diploma",
                                                "Apprenticeship certificate",
                                                "Non-apprenticeship trades certificate or diploma"),
                             Associate = c("Postsecondary certificate or diploma below bachelor level",
                                           "University certificate or diploma below bachelor level"),
                             `Bachelor+` = c("University certificate, diploma or degree at bachelor level or above",
                                             "College, CEGEP or other non-university certificate or diploma",
                                             "Postsecondary certificate, diploma or degree",
                                             "Bachelor’s degree or higher",
                                             "Bachelor's degree",
                                             "University certificate or diploma above bachelor level",
                                             "Degree in medicine, dentistry, veterinary medicine or optometry",
                                             "Master's degree",
                                             "Earned doctorate")
    ))

## add presence of scs site
load("data/processed/final_scs.rda")
total_labor$scs <- 0
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
    # big problem. These cities aren't in the labor measures in 2016 or 2021