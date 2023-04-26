census_2021_og <- read_csv("~/Desktop/Senior Year/Thesis/Data/General_City_Data.csv")
census_2016_og <- read_csv("/Users/stuartsumner/Desktop/Senior Year/Thesis/Data/98-401-X2016042_English_CSV_data.csv")
census_2016_og <- census_2016_og %>% 
  filter(GEO_LEVEL != 2)

# Finding duplicate sets
duplicates <- census_2016_og %>% 
  count(GEO_NAME) %>% 
  filter(n != 2247) %>% 
  pull(GEO_NAME)

dup_set <- census_2016_og %>% 
  filter(GEO_NAME %in% duplicates)

duplicates_2021 <- census_2021_og %>% 
  count(GEO_NAME) %>% 
  filter(n != 2631)
  # No duplicates for 2021 set?

census_2016 <- anti_join(census_2016_og, dup_set)

colnames <- c("year", "geo", "type", "count_total", "count_men", "count_women")

census_2016 <- census_2016 %>% 
  janitor::clean_names() %>% 
  select(census_year, geo_name, starts_with("dim"))
colnames(census_2016) <- colnames
census_2016 <- census_2016 %>% 
  mutate(
    count_total = as.numeric(count_total),
    count_men = as.numeric(count_men),
    count_women = as.numeric(count_women)
  )

ugh <-  census_2016 %>% 
  filter(geo == "Dorchester") %>% 
  filter(type == "Population, 2016")

census_2021 <- census_2021_og %>% 
  janitor::clean_names() %>% 
  select(census_year, geo_name, characteristic_name, c1_count_total, c2_count_men, c3_count_women)
colnames(census_2021) <- colnames
census_2021 <- census_2021 %>% 
  filter(type != "Population, 2016")

census_both <- bind_rows(census_2016, census_2021)

city_list <- distinct(census_2016, geo)
type_list_2016 <- distinct(census_2016, type)

types_of_interest <- c("Population, 2011", "Population, 2016", "Population, 2021", "Population density per square kilometre", "Land area in square kilometres", "Total private dwellings", "Average age of the population", "Average household size", "Married", "Average size of census families", "Median total income in 2020 among recipients ($)", "Gini index on adjusted household total income", "In bottom half of the distribution", "Unemployment rate")

attempt1 <- census_both %>% 
  filter(type %in% types_of_interest) %>% 
  select(year, geo, type, count_total) %>% 
  pivot_wider(names_from = type, values_from = count_total) %>% 
  janitor::clean_names()

tidied <- attempt1 %>% 
  transmute(
    year = attempt1$year,
    geo = attempt1$geo,
    population_2011 = as.numeric(attempt1$population_2011),
    population_2016 = as.numeric(attempt1$population_2016),
    population_2021 = as.numeric(attempt1$population_2021),
    total_private_dwellings = as.numeric(attempt1$total_private_dwellings),
    population_density = as.numeric(attempt1$population_density_per_square_kilometre),
    land_area = as.numeric(attempt1$land_area_in_square_kilometres),
    avg_age = as.numeric(attempt1$average_age_of_the_population),
    avg_household_size = as.numeric(attempt1$average_household_size),
    married = as.numeric(attempt1$married),
    avg_family_size = as.numeric(attempt1$average_size_of_census_families),
    med_income = as.numeric(attempt1$median_total_income_in_2020_among_recipients),
    income_lower_half = as.numeric(attempt1$in_bottom_half_of_the_distribution),
    gini = as.numeric(attempt1$gini_index_on_adjusted_household_total_income),
    unemployment = as.numeric(attempt1$unemployment_rate)
  )


census <- read_csv("Data/tidied_census.csv")
scs <- read_csv("Data/SCS_Loc.csv") %>% 
  janitor::clean_names()

scs_city <- distinct(scs, city) %>% 
  pull()

final_census <- final_census %>% 
  mutate(
    scs = factor(scs)
  )

census <- readRDS("Data/final_census.rds")
