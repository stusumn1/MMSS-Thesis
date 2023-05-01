library(tidyverse)

## census 2021 processing----
census_2021_og <- read_csv("data/raw/census_2021.csv") %>% 
  janitor::clean_names() %>% 
  separate(geo_name, into = c("geo_name", "geo_type"), sep = ", ") %>% 
  select(census_year, geo_level, geo_name, geo_type, characteristic_name, c1_count_total)

types_of_interest <- c("Population, 2021", "Population percentage change, 2011 to 2016", "Population percentage change, 2016 to 2021", "Population density per square kilometre", "Land area in square kilometres", "Total private dwellings", "Average age of the population", "Average household size", "Married", "Average size of census families", "Median total income in 2015 among recipients ($)", "Median total income in 2020 among recipients ($)", "In bottom half of the distribution", "In the bottom half of the distribution", "Gini index on adjusted household total income", "Unemployment rate")
census_2021_test <- census_2021_og %>% 
  #mutate(geo_name = flat_names) %>% 
  filter(characteristic_name %in% types_of_interest, geo_name != "Lloydminster (Part)" & geo_name != "L'Ange-Gardien, Municipalit� (M�)") %>% 
  pivot_wider(names_from = characteristic_name, values_from = c1_count_total) %>% 
  janitor::clean_names()
census_2021 <- census_2021_test %>% 
  transmute(
    year = census_2021_test$census_year,
    geo = census_2021_test$geo_name,
    population = as.numeric(census_2021_test$population_2021),
    population_change = as.numeric(census_2021_test$population_percentage_change_2016_to_2021),
    total_private_dwellings = as.numeric(census_2021_test$total_private_dwellings),
    population_density = as.numeric(census_2021_test$population_density_per_square_kilometre),
    land_area = as.numeric(census_2021_test$land_area_in_square_kilometres),
    avg_age = as.numeric(census_2021_test$average_age_of_the_population),
    avg_household_size = as.numeric(census_2021_test$average_household_size),
    married = as.numeric(census_2021_test$married),
    avg_family_size = as.numeric(census_2021_test$average_size_of_census_families),
    med_income = as.numeric(census_2021_test$median_total_income_in_2020_among_recipients),
    income_lower_half = as.numeric(census_2021_test$in_bottom_half_of_the_distribution),
    gini = as.numeric(census_2021_test$gini_index_on_adjusted_household_total_income),
    unemployment = as.numeric(census_2021_test$unemployment_rate)
  )
census_2021_test %>% 
  colnames()

## census 2016 processing ----
census_2016_og <- read_csv("data/raw/census_2016.csv") %>% 
  janitor::clean_names() %>% 
  select(1, 3, 4, 8, 10, 13)
names <- c("year", "geo_level", "geo", "csd_type", "characteristic_name", "c1_count_total")
colnames(census_2016_og) <- names
types_of_interest2 <- c("Population, 2016", "Population percentage change, 2011 to 2016", "Population percentage change, 2016 to 2021", "Population density per square kilometre", "Land area in square kilometres", "Total private dwellings", "Average age of the population", "Average household size", "Married", "Average size of census families", "Median total income in 2015 among recipients ($)", "Median total income in 2020 among recipients ($)", "In bottom half of the distribution", "In the bottom half of the distribution", "Gini index on adjusted household total income", "Unemployment rate")
census_2016_test <- census_2016_og %>% 
  filter(characteristic_name %in% types_of_interest2, geo != "Lloydminster (Part)", geo != "L'Ange-Gardien") %>% 
  pivot_wider(names_from = characteristic_name, values_from = c1_count_total) %>% 
  janitor::clean_names()
?gsub
census_2016 <- census_2016_test%>% 
  transmute(
    year = census_2016_test$year,
    geo = census_2016_test$geo,
    #geo_level = census_2016_test$geo_level,
    population = as.numeric(census_2016_test$population_2016),
    population_change = as.numeric(census_2016_test$population_percentage_change_2011_to_2016),
    total_private_dwellings = as.numeric(census_2016_test$total_private_dwellings),
    population_density = as.numeric(census_2016_test$population_density_per_square_kilometre),
    land_area = as.numeric(census_2016_test$land_area_in_square_kilometres),
    avg_age = as.numeric(census_2016_test$average_age_of_the_population),
    avg_household_size = as.numeric(census_2016_test$average_household_size),
    married = as.numeric(census_2016_test$married),
    avg_family_size = as.numeric(census_2016_test$average_size_of_census_families),
    med_income = as.numeric(census_2016_test$median_total_income_in_2015_among_recipients),
    income_lower_half = as.numeric(census_2016_test$in_the_bottom_half_of_the_distribution),
    unemployment = as.numeric(census_2016_test$unemployment_rate)
  )
census_2016 %>% 
  colnames()
census_2021 %>% 
  colnames()

## census 2011 processing ----
census_2011_og <- read_csv("data/raw/census_2011.csv", skip = 1)
census_2011_og$year <- rep.int(2011, 2479416)

names <- c("year","geo_code",  "province", "geo", "cd_name", "csd_type", "characteristic_name", "c1_count_total")
types_of_interest4 <- c("Population in 2011", "2006 to 2011 population change (%)", "Population density per square kilometre", "Total private dwellings", "Land area (square km)", "Married couples", "Average number of persons in private households", "Median age of the population", "Average number of persons per census family")
no_good <- c("SECHELT (PART)", "OKANAGAN (PART) 1", "LONG PLAIN (PART) 6", "SIMONDS", "BEAR RIVER (PART) 6", "NEW CREDIT (PART) 40A", "SIX NATIONS (PART) 40", "WHITEFISH RIVER (PART) 4", "FORT ALBANY (PART) 67", "SAINT-ELZEAR", "SAINT-SIMEON", "SAINT-LEON-LE-GRAND", "SAINT-DAMASE", "SAINTE-FELICITE", "SAINT-SIMON", "SAINT-SEVERIN", "SAINTE-SABINE", "NOTRE-DAME-DE-LOURDES", "NOTRE-DAME-DU-MONT-CARMEL", "SAINT-STANISLAS", "SAINTE-MONIQUE")
colnames(census_2011_og)
census_2011_og <- census_2011_og %>% 
  select(15, 1:5, 7, 9)
colnames(census_2011_og) <- names

census_2011_og %>% 
  distinct(characteristic_name) %>% 
  view()

census_2011_test <- census_2011_og %>% 
  filter(characteristic_name %in% types_of_interest4) %>% 
  filter(!(geo %in% no_good)) %>% 
  #select(-5) %>% 
  pivot_wider(names_from = characteristic_name, values_from = "c1_count_total") %>% 
  janitor::clean_names()

census_2011 <- census_2011_test %>% 
  transmute(
    year = census_2011_test$year,
    geo = census_2011_test$cd_name,
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
  # reduce 2011 census to locales with data from future censuses
census_2011 %>% 
  distinct(geo)
geo_2016 <- census_2016 %>% 
  distinct(geo) %>% 
  pull()
c2011 <- census_2011 %>% 
  filter(geo %in% geo_2016)

census <- bind_rows(c2011, census_2016, census_2021)
save(census, file = "data/processed/census_1121.rda")
