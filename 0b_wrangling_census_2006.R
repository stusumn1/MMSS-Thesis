library(tidyverse)

## joining provinces ----
alta <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-ALTA.csv") %>% 
  janitor::clean_names()
bc <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-BC.csv") %>% 
  janitor::clean_names()
man <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-MAN.csv") %>% 
  janitor::clean_names()
nb <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-NB.csv") %>% 
  janitor::clean_names()
nl <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-NL.csv") %>% 
  janitor::clean_names()
ns <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-NS.csv") %>% 
  janitor::clean_names()
nvt <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-NVT.csv") %>% 
  janitor::clean_names()
nwt <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-NWT.csv") %>% 
  janitor::clean_names()
ont <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-ONT.csv") %>% 
  janitor::clean_names()
pei <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-PEI.csv") %>% 
  janitor::clean_names()
que <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-QUE.csv") %>% 
  janitor::clean_names()
sask <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-SASK.csv") %>% 
  janitor::clean_names()
yt <- read_csv(skip = 1, file = "data/raw/census_2006/93F0053XIE-301-YT.csv") %>% 
  janitor::clean_names()

census_2006_og <- bind_rows(alta, bc, man, nb, nl, ns, nvt, nwt, ont, pei, que, sask, yt)
census_2006_og$year <- rep.int(2006, 1093681)

#save(census_2006_og, file = "data/raw/census_2006.rda")

names <- c("year", "province", "geo", "csd_type", "topic", "characteristic_name", "c1_count_total")
types_of_interest3 <- c("Total - All persons", "Population density per square kilometre", "Land area (square km)", "Total private dwellings", "Median age of the population", "Median total income of persons 15 years of age and over ($)", "Married", "Average number of persons in married-couple families", "1996 to 2001 population change (%)", "Unemployment rate")
no_good <- c("SECHELT (PART)", "OKANAGAN (PART) 1", "LONG PLAIN (PART) 6", "SIMONDS", "BEAR RIVER (PART) 6", "NEW CREDIT (PART) 40A", "SIX NATIONS (PART) 40", "WHITEFISH RIVER (PART) 4", "FORT ALBANY (PART) 67", "SAINT-ELZEAR", "SAINT-SIMEON", "SAINT-LEON-LE-GRAND", "SAINT-DAMASE", "SAINTE-FELICITE", "SAINT-SIMON", "SAINT-SEVERIN", "SAINTE-SABINE", "NOTRE-DAME-DE-LOURDES", "NOTRE-DAME-DU-MONT-CARMEL", "SAINT-STANISLAS", "SAINTE-MONIQUE")

census_2006_og <- census_2006_og %>% 
  select(13, 2, 4, 5, 6, 7, 9)
colnames(census_2006_og) <- names

census_2006_test <- census_2006_og %>% 
  filter(characteristic_name %in% types_of_interest3 & topic != "Language(s) First Learned and Still Understood" & topic != "Immigration Characteristics" & topic != "Aboriginal Population") %>% 
  filter(!(geo %in% no_good)) %>% 
  select(-5) %>% 
  pivot_wider(names_from = characteristic_name, values_from = c1_count_total) %>% 
  janitor::clean_names()

census_2006 <- census_2006_test %>% 
  transmute(
    year = census_2006_test$year,
    geo = census_2006_test$geo,
    population = as.numeric(census_2006_test$total_all_persons),
    population_change = as.numeric(census_2006_test$x1996_to_2001_population_change_percent),
    total_private_dwellings = as.numeric(census_2006_test$total_private_dwellings),
    population_density = as.numeric(census_2006_test$population_density_per_square_kilometre),
    land_area = as.numeric(census_2006_test$land_area_square_km),
    avg_age = as.numeric(census_2006_test$median_age_of_the_population),
    avg_household_size = as.numeric(census_2006_test$average_number_of_persons_in_married_couple_families),
    married = as.numeric(census_2006_test$married),
    avg_family_size = as.numeric(census_2006_test$average_number_of_persons_in_married_couple_families),
    med_income = as.numeric(census_2006_test$median_total_income_of_persons_15_years_of_age_and_over),
    unemployment = as.numeric(census_2006_test$unemployment_rate)
  )

last_three <- bind_rows(census_2006, last_two) %>% 
  filter(avg_family_size != 0 & !is.na(avg_family_size))
  ## need to find a way to lowercase the city names so they line up with scs