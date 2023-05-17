library(tidyverse)
library(tidymodels)
library(lubridate)
library(ranger)
library(kknn)

# Struggling to convert ref_date to datetime column


#---------------------

census_2011 <- read_csv("data/census_2011.csv", skip = 1)

y <- list.files(path = "data/raw/census_2011/", full.names = T) %>% 
  map(read_csv)

# read in all crime csv and put in 1 tibble
temp_data <- list.files(path = "data/raw/Crime City/", full.names =  TRUE) %>% 
  map(read_csv) 

rm(temp_data)

crime_city <- bind_rows(temp_data) %>% 
  janitor::clean_names() %>% 
  separate(geo, into = c("City", "Province"), sep = ", ") %>% 
  separate(Province, into = c("Province", NA), sep = " ") %>% 
  select(ref_date, City, Province, statistics, value) %>% 
  pivot_wider(names_from = statistics, values_from = value) %>% 
  select(-9:-14) %>% 
  janitor::clean_names()


rm(crime_data)

temp_crime_province <- list.files(path = "data/raw/Crime Province/", full.names = T) %>% 
  map(read_csv)

crime_province <- bind_rows(temp_crime_province) %>% 
  janitor::clean_names() %>% 
  separate(geo, into = c("geo", NA), sep = " ") %>% 
  select(ref_date, geo, statistics, value) %>% 
  pivot_wider(names_from = statistics, values_from = value) %>% 
  janitor::clean_names()

rm(temp_crime_province)

temp_labor_quebec <- list.files(path = "Data/Labor/Quebec", full.names = T) %>% 
  map(read_csv)

labor_quebec <- bind_rows(temp_labor_quebec) %>% 
  janitor::clean_names() %>% 
  select(ref_date, geo, labour_force_characteristics, age_group, value) %>% 
  pivot_wider(names_from = labour_force_characteristics, values_from = value)

rm("temp_labor", "temp_labor_al", "temp_labor_bc", "temp_labor_ont", "temp_labor_que", "temp_labor_sas")

labor <- bind_rows(labor_al, labor_bc, labor_ontario, labor_quebec, labor_sas) %>% 
  janitor::clean_names()

labor1 <- labor %>% 
  as.POSIXct(labor[[1]], tz = "", format = "%Y-%m")

?as.POSIXct
?as_date
as_date(labor[[1]], format = "%Y-%m")

scs <- read_excel("Supervised Consumption Sites.xlsx")

write_csv(scs, "Data/SCS_Loc.csv")
write_csv(labor, "Data/Labor.csv")
write_csv(crime_city, "Data/Crime_City.csv")
write_csv(crime_province, "Data/Crime_Province.csv")


#-------------------------

dec_22 <- read_csv("/Users/stuartsumner/Downloads/1410028703_databaseLoadingData.csv")
scs <- read_excel("Data/Supervised Consumption Sites.xlsx")
crime_calgary <- read_csv("Crime_Calgary.csv")
labor_canada <- read_csv("Data/Labor/Canada/Canada_Q1_19.csv")
?list.files
crime_city_names <- list.files("~/Desktop/Senior Year/Thesis/Data/Crime City")

# Pre-processing one set of data, to be repeated for all in the folder

Crime_Toronto <- read_csv("Crime_Toronto.csv")

names(Crime_Toronto) <- tolower(colnames(Crime_Toronto))

h <- select(Crime_Toronto, ref_date, geo, statistics, uom, value) %>% 
  arrange(ref_date)



?get
get("crime_canada")

# Reading in 'Crime City' folder-- I want to separate each file and put it separately in our environment

ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv")
for (i in 1:length(listcsv)){
  ldf[[i]] <- read_csv(listcsv[i])
}

grey <- lapply(listcsv, read_csv)

crime_calgary <- grey[[1]]

crime_calgary <- tibble(ldf[[1]])

crime_edmonton <- grey[[2]]

crimenames <- c("crime_calgary", "crime_edmonton", "crime_greatersudbury", "crime_guelph", "crime_hamilton", "crime_kingston", "crime_kitchener", "crime_london", "crime_montreal", "crime_ottawa", "crime_saskatoon", "crime_stcatharines", "crime_thunderbay", "crime_toronto", "crime_vancouver", "crime_victoria")

white <- function(x) {
  names(grey[[x]]) <- crimenames[[x]]
}

for (i in 1:length(crimenames)){
  i <- grey[[i]]
}

white <- function(x) {
  
}


# Winner
test_process <- Crime_Toronto %>% 
  select(ref_date, geo, statistics, uom, value) %>% 
  pivot_wider(names_from = ref_date, values_from = value)

# Create a loop to create individual objects from folder and process each like above

# Creating an object for each .csv in "Crime City" folder
names(ldf) <- crimenames
tibble(ldf[[1]])

tbl <-
  list.files(pattern = "*.csv")

names <-substr(tbl, 1, nchar(tbl) - 4)

for(i in names){
  filepath <- file.path("~/Desktop/Senior Year/Thesis/Data/Crime City", paste(i, ".csv", sep = ""))
  assign(i, read.delim(filepath,
                       colClasses = c(rep("character", 15)),
                       sep = ","))
}

# Use get() to process each dataset in a loop
colnames(crime_calgary) <- names(crime_calgary) %>% 
  tolower()

crime_calgary <- get("crime_calgary") %>% 
  select(ref_date, geo, statistics, uom, value) %>% 
  pivot_wider(names_from = ref_date, values_from = value)


for (i in names){
  xx <- get(i)
  colnames(xx) <- tolower(names(i))
  i <- i %>% select(ref_date, geo, statistics, uom, value) %>% 
    pivot_wider(names_from = ref_date, values_from = value)
}

#select(ref_date, geo, statistics, uom, value) %>% 
  #pivot_wider(names_from = ref_date, values_from = value)

#----------------

labor <- read_csv("Data/Labor.csv")
crime_city <- read_csv("Data/Crime_City.csv")
crime_province <- read_csv("Data/Crime_Province.csv")
scs_loc <- read_csv("Data/SCS_Loc.csv")

crime_city %>% 
  group_by(province) %>% 
  ggplot(aes(ref_date, non_violent_crime_severity_index)) +
  geom_line(aes(color = city))

crime_province %>% 
  ggplot(aes(ref_date, non_violent_crime_severity_index)) +
  geom_line(aes(color = geo))

labor %>% 
  filter(age_group == "15 years and over") %>% 
  ggplot(aes(ref_date, participation_rate)) +
  geom_line()

labor1 <- labor %>% 
  mutate(
    ref_date = as.POSIXct(ref_date, format = "%Y-%m")
  )

labor <- labor %>% 
  mutate(
    ref_date = zoo::as.yearmon(ref_date)
  )

#-------------

colnames <- c("year", "geo", "type", "count_total", "rate_total", "count_men", "count_women", "rate_men", "rate_women")

city_gen <- read_csv("~/Desktop/Senior Year/Thesis/Data/General_City_Data.csv") %>% 
  janitor::clean_names() %>% 
  select(census_year, geo_name, characteristic_name, ends_with("total"), ends_with("men"))
colnames(city_gen) <- colnames

city_list <- distinct(city_gen, geo)
type_list <- distinct(city_gen, type)

types_of_interest <- c("Population, 2021", "Population, 2016", "Population density per square kilometre", "Land area in square kilometres", "Total private dwellings", "Average age of the population", "Average household size", "Married", "Average size of census families", "Median total income in 2020 among recipients ($)", "Gini index on adjusted household total income", "In bottom half of the distribution", "No certificate, diploma or degree", "Unemployment rate")

sml_city_gen <- city_gen %>% 
  filter(type %in% types_of_interest) %>% 
  select(geo, type, count_total) %>% 
  pivot_wider(names_from = type, values_from = count_total) %>% 
  janitor::clean_names()

sml_city_gen <- sml_city_gen %>% 
  transmute(
    geo = sml_city_gen$geo,
    population_2021 = as.numeric(sml_city_gen$population_2021),
    total_private_dwellings = as.numeric(sml_city_gen$total_private_dwellings),
    population_density = as.numeric(sml_city_gen$population_density_per_square_kilometre),
    land_area = as.numeric(sml_city_gen$land_area_in_square_kilometres),
    avg_age = as.numeric(sml_city_gen$average_age_of_the_population),
    avg_household_size = as.numeric(sml_city_gen$average_household_size),
    married = as.numeric(sml_city_gen$married),
    avg_family_size = as.numeric(sml_city_gen$average_size_of_census_families),
    med_income = as.numeric(sml_city_gen$median_total_income_in_2020_among_recipients),
    income_lower_half = as.numeric(sml_city_gen$in_bottom_half_of_the_distribution),
    gini = as.numeric(sml_city_gen$gini_index_on_adjusted_household_total_income),
    unemployment = as.numeric(sml_city_gen$unemployment_rate)
  )

write_csv(sml_city_gen, file = "~/Desktop/Senior Year/Thesis/Data/Small_City.csv")

# I want to include education but they all seem to be list types, figure out how to split

sml_city_gen %>% 
  ggplot(aes(unemployment)) +
  geom_density()


# Need to create dummy variable for presence of scs

census <- read_csv("Data/tidied_census.csv")
scs <- read_csv("Data/SCS_Loc.csv") %>% 
  janitor::clean_names()

scs_city <- distinct(scs, city) %>% 
  pull()

census <- census %>% 
  mutate(
    scs = ifelse(
      geo %in% scs_city, 1, 0
    )
  )


#--------------

# Some preliminary analysis

city_split <- initial_split(sml_city_gen, .75, strata = unemployment)
city_train <- training(city_split)
city_test <- testing(city_split)

city_folds <- vfold_cv(city_train, v = 5, repeats = 5)

# Linear regression
lm_model <- linear_reg(engine = "lm", mode = "regression")
recipe <- recipe(unemployment ~ ., data = sml_city_gen) %>% 
  step_rm(geo) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())
lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(recipe)
lm_fit <- fit(lm_wflow, city_train)
predict(lm_fit, city_test) %>% 
  bind_cols(city_test$unemployment) %>% 
  ggplot(aes(.pred, ...2)) +
  geom_point(size = 2) +
  geom_abline(color = "red") +
  coord_cartesian(xlim = c(3.5, 14)) +
  labs(x = "Pred", y = "Observed") +
  theme_minimal()

predict(lm_fit, city_test) %>% 
  bind_cols(city_test$unemployment) %>% 
  rmse(.pred, ...2)

lm_fit_v <- fit_resamples(lm_wflow, resamples = city_folds)
collect_metrics(lm_fit_v)

# Random forest
rf_model <- rand_forest(engine = "ranger", mode = "regression", trees = 1000, min_n = 10)
rf_wflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe)
rf_fit <- fit(rf_wflow, city_train)
predict(rf_fit, city_test) %>% 
  bind_cols(city_test$unemployment) %>% 
  autoplot()
  rmse(.pred, ...2)

rf_fit_v <- fit_resamples(rf_wflow, resamples = city_folds)
collect_metrics(rf_fit_v)

# K-nearest neighbors
knn <-
  nearest_neighbor(neighbors = 40) %>%
  set_engine('kknn') %>%
  set_mode('regression')

knn_wflow <- workflow() %>% 
  add_model(knn) %>% 
  add_recipe(recipe)

knn_fit_v <- fit_resamples(knn_wflow, city_folds)
collect_metrics(knn_fit_v)

# I need more data! Just not enough

# Census analysis

census <- readRDS("data/processed/final_census.rds")

census_split <- initial_split(census, .8, strata = scs)

census_train <- training(census_split)
census_test <- testing(census_split)
census_folds <- vfold_cv(census_train, v = 10, repeats = 3, strata = scs)

# Establish recipe
recipe <- recipe(scs ~ ., data = census_train) %>% 
  step_rm(med_income, income_lower_half, gini, population_last, geo) %>% 
  step_impute_mean(population_current, total_private_dwellings, population_density, avg_age, avg_household_size, married, avg_family_size, unemployment) %>% 
  step_pca() %>% 
  step_normalize(starts_with("population"), starts_with("avg"), land_area, lat, lon, unemployment) %>% 
  prep()

skimr::skim(census)

bake(recipe, new_data = NULL)

# Establish models
rf_model <- rand_forest(mode = "regression",
                        min_n = tune(),
                        mtry = tune()) %>% 
  set_engine("ranger")

tree_model <- boost_tree(mode = "regression",
                         min_n = tune(),
                         mtry = tune(),
                         learn_rate = tune()) %>% 
  set_engine("xgboost")

knn_model <- nearest_neighbor(mode = "regression",
                              neighbors = tune()) %>% 
  set_engine("kknn")

# Set hyperparameter tuning
rf_params <- extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(c(1, 13))) 
rf_grid <- grid_regular(rf_params, levels = 5)

tree_params <- extract_parameter_set_dials(tree_model) %>% 
  update(mtry = mtry(c(1, 13))) 
tree_grid <- grid_regular(tree_params, levels = 5)

knn_params <- extract_parameter_set_dials(knn_model)
knn_grid <- grid_regular(knn_params, levels = 5)  
  
# Make workflows
rf_wkflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe)

tree_wkflow <- workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(recipe)

knn_wkflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(recipe)

#Optimize tunings
rf_tuned <- rf_wkflow %>% 
  tune_grid(census_folds, grid = rf_grid)

saveRDS(rf_tuned, file = "fitted/rf_tuned.rds")

tree_tuned <- tree_wkflow %>% 
  tune_grid(census_folds, grid = tree_grid)

saveRDS(tree_tuned, file = "fitted/tree_tuned.rds")

knn_tuned <- knn_wkflow %>% 
  tune_grid(census_folds, grid = knn_grid)

saveRDS(knn_tuned, file = "fitted/knn_tuned.rds")

autoplot(rf_tuned, metric = "rmse")
autoplot(knn_tuned, metric = "rmse")

# Fitting winning model
select_best(rf_tuned, metric = "rmse")
select_best(knn_tuned, metric = "rmse")

show_best(rf_tuned) %>% select(-.estimator)
show_best(knn_tuned) %>% select(-.estimator)

rf_wkflow_tuned <- rf_wkflow %>% 
  finalize_workflow(select_best(rf_tuned, metric = "rmse"))

rf_results <- fit(rf_wkflow_tuned, census_train)

census_metric <- metric_set(rmse)

rf_pred <- predict(rf_results, new_data = census_test) %>% 
  bind_cols(census_test %>% select(scs))

rf_pred %>% 
  census_metric(truth = scs, estimate = .pred)

ggplot(rf_pred) +
  geom_point(aes(.pred, scs), size = 2, alpha = .7) +
  geom_abline(color = "red", lwd = 2, alpha = .5) +
  labs(
    x = "Predicted",
    y = "Actual"
  )

library(poissonreg)

log_lin_spec <- poisson_reg()

log_lin_fit <- 
  log_lin_spec %>% 
  fit(scs ~ ., data = census_train)
log_lin_fit

metrics <- collect_metrics(rf_tuned) %>% 
  filter(mean == max(mean))

view(metrics)

## what happened to the old data set? Can't process accents on letters, screwing everything up
unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

#library(gsubfn)
#string="Hølmer"
#flat_names <- gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array, census_2021_og$geo_name)
load("fitted/flat_names.rda")
flat_names
Encoding(census_2021_og$geo_name) <- "UTF-8"

namb <- scan(file='/Users/stuartsumner/Downloads/98-401-X2021003_eng_CSV (1)/98-401-X2021003_English_CSV_data.csv', fileEncoding='UTF-8',
             what=character(), sep=',', allowEscapes=T)
cat(namb)

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

family_1116 %>% 
  filter(geo %in% geos_21) %>% 
  view()

family_1116_og %>% 
  select(12, 15) %>% 
  view()

## faily data in 2011 and 2016
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
family_total <- bind_rows(family_21_test1, family_16_test) %>% 
  arrange(year) %>% 
  mutate(
    prop_two_parent = two_parent/total
  )
family_total

geos_21 <- family_21_test %>% 
  distinct(geo) %>% 
  pull()

family_1116 %>% 
  ungroup() %>% 
  distinct(geo)

## problem with geos overlapping -- 11 and 16 are census subdivisions, 21 is metros and agglomerations
## check out full dataset (but can't discriminate by age)

family_total_og <- read_csv("data/raw/family_total_big.csv") %>% 
  janitor::clean_names()
family_total_test <- family_total_og %>% 
  filter(census_family_structure_7a %in% c("Total - Census family structure", "Total - Couple families", "Total - One-parent families"))

