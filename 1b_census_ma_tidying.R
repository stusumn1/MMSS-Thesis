# Tidying data for census 2011-2021

library(tidyverse)

## census 2021 processing----
census_2021_og <- read_csv("data/raw/census_2021_little.csv") %>% 
  janitor::clean_names() %>% 
  #separate(geo_name, into = c("geo_name", "geo_type"), sep = ", ") %>% 
  #filter(!is.na(geo_type)) %>% 
  select(census_year, geo_name, characteristic_name, c1_count_total)

types_of_interest <- c("Population, 2021", "Population percentage change, 2011 to 2016", "Population percentage change, 2016 to 2021", "Population density per square kilometre", "Land area in square kilometres", "Total private dwellings", "Average age of the population", "Average household size", "Married", "Average size of census families")
census_2021_test <- census_2021_og %>% 
  #mutate(geo_name = flat_names) %>% 
  filter(characteristic_name %in% types_of_interest) %>% 
  pivot_wider(names_from = characteristic_name, values_from = c1_count_total) %>% 
  janitor::clean_names() %>% 
  # remove problematic row L'Ange Gardien
  slice(-602)

census_2021 <- census_2021_test %>% 
  transmute(
    year = factor(census_2021_test$census_year),
    geo = census_2021_test$geo_name,
    population = as.numeric(census_2021_test$population_2021),
    population_change = as.numeric(census_2021_test$population_percentage_change_2016_to_2021),
    total_private_dwellings = as.numeric(census_2021_test$total_private_dwellings),
    population_density = as.numeric(census_2021_test$population_density_per_square_kilometre),
    land_area = as.numeric(census_2021_test$land_area_in_square_kilometres),
    avg_age = as.numeric(census_2021_test$average_age_of_the_population),
    avg_household_size = as.numeric(census_2021_test$average_household_size),
    married = as.numeric(census_2021_test$married),
    avg_family_size = as.numeric(census_2021_test$average_size_of_census_families)
  )
  # this is definitely prettier, just very small. This is 160, other is ~850

## census 2016 processing ----
census_2016_og <- read_csv("data/raw/census_2016_little.csv") %>% 
  janitor::clean_names() %>% 
  #filter(!is.na(csd_type_name)) %>% 
  select(1, 4, 9, 12)
names <- c("year", "geo", "characteristic_name", "c1_count_total")
colnames(census_2016_og) <- names
types_of_interest2 <- c("Population, 2016", "Population percentage change, 2011 to 2016", "Population percentage change, 2016 to 2021", "Population density per square kilometre", "Land area in square kilometres", "Total private dwellings", "Average age of the population", "Average household size", "Married", "Average size of census families")
census_2016_test <- census_2016_og %>% 
  filter(characteristic_name %in% types_of_interest2) %>% 
  pivot_wider(names_from = characteristic_name, values_from = c1_count_total) %>% 
  janitor::clean_names()

census_2016 <- census_2016_test%>% 
  transmute(
    year = factor(census_2016_test$year),
    geo = census_2016_test$geo,
    population = as.numeric(census_2016_test$population_2016),
    population_change = as.numeric(census_2016_test$population_percentage_change_2011_to_2016),
    total_private_dwellings = as.numeric(census_2016_test$total_private_dwellings),
    population_density = as.numeric(census_2016_test$population_density_per_square_kilometre),
    land_area = as.numeric(census_2016_test$land_area_in_square_kilometres),
    avg_age = as.numeric(census_2016_test$average_age_of_the_population),
    avg_household_size = as.numeric(census_2016_test$average_household_size),
    married = as.numeric(census_2016_test$married),
    avg_family_size = as.numeric(census_2016_test$average_size_of_census_families)
  )

## census 2011 processing ----
census_2011_og <- read_csv("data/raw/census_2011_little.csv", skip = 1) %>% 
  janitor::clean_names()
census_2011_og$year <- rep.int(2011, 73160)
census_2011_og <- census_2011_og %>% 
  select(15, 3, 6, 8)

names <- c("year","geo", "characteristic_name", "c1_count_total")
colnames(census_2011_og) <- names

types_of_interest3 <- c("Population in 2011", "2006 to 2011 population change (%)", "Population density per square kilometre", "Total private dwellings", "Land area (square km)", "Married couples", "Average number of persons in private households", "Median age of the population", "Average number of persons per census family")

census_2011_test <- census_2011_og %>% 
  filter(characteristic_name %in% types_of_interest3) %>% 
  pivot_wider(names_from = characteristic_name, values_from = c1_count_total) %>% 
  janitor::clean_names()

census_2011 <- census_2011_test %>% 
  transmute(
    year = factor(census_2011_test$year),
    geo = census_2011_test$geo,
    population = as.numeric(census_2011_test$population_in_2011),
    population_change = as.numeric(census_2011_test$x2006_to_2011_population_change_percent),
    total_private_dwellings = as.numeric(census_2011_test$total_private_dwellings),
    population_density = as.numeric(census_2011_test$population_density_per_square_kilometre),
    land_area = as.numeric(census_2011_test$land_area_square_km),
    avg_age = as.numeric(census_2011_test$median_age_of_the_population),
    avg_household_size = as.numeric(census_2011_test$average_number_of_persons_per_census_family),
    married = as.numeric(census_2011_test$married_couples),
    avg_family_size = as.numeric(census_2011_test$average_number_of_persons_per_census_family),
  )

## combining census data ----
scs <- readRDS("data/processed/final_scs.rds") %>% 
  janitor::clean_names()
  # extract list of cities with scs clinics
scs_city <- distinct(scs, city) %>% 
  pull()
census_2011$scs <- 0
census_2016$scs <- 0
  # add interruption variable to 2021 Census data
census_2021 <- census_2021 %>% 
  mutate(
    scs = ifelse(
      geo %in% scs_city, 1, 0
    )
  )

census_little <- bind_rows(census_2011, census_2016, census_2021)
#save(census_little, file = "data/processed/census_1121_little.rda")
