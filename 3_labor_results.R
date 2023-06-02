## extracting ITS labor results

## load packages----
library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(its.analysis)
library(tictoc)
library(datawizard)
library(gt)

## handle common conflicts
tidymodels_prefer()

## setup parallel processing
registerDoMC(cores = 8)

## seed
set.seed(4444)

## read in result objects----
load("results/prop_part_uneduc.rda")
load("data/processed/labor_census_little.rda")

## proportion educated below high school level
labor_census <- labor_census %>% 
  filter(unemployed_uneduc != 0)

# feature engineering/recipe
recipe <- recipe(prop_part_uneduc ~ ., labor_census) %>% 
  step_rm(geo, land_area, total, total_uneduc, unemployed_total, unemployed_uneduc, participation_total, participation_uneduc, avg_family_size) %>% 
  step_filter_missing(all_predictors(), threshold = .15) %>% 
  step_impute_knn(all_predictors()) %>% 
  #step_log(married, population, population_density, total_private_dwellings, unemployment_rt_total, unemployment_rt_uneduc) %>% 
  step_normalize(avg_age, avg_household_size, population_change) %>%
  step_zv() %>% 
  prep()

## proportion participating in labor force educated below high school 
part_uneduc_dat <- bake(recipe, new_data = NULL) %>% 
  mutate(scs = factor(scs))

labor_anova <- aov(prop_part_uneduc ~ . -scs - population, data = part_uneduc_dat)
labor_anova %>% 
  summary()
labor_tukey <- TukeyHSD(labor_anova)
plot(labor_tukey) 

labor_summary <- data.frame(Covariate = c("year", "population % change", "private dwellings", "population density", "average age",  "average household size", "number married \ncouples", "unemployment rate", "unemployment rate*", "total*", "proportion unemployed*", "residual"), unclass(summary(labor_anova)),  # Convert summary to data frame
                             check.names = F)
labor_summary %>% 
  slice(-1, -10, -12) %>% 
  gt() %>% 
  gt::cols_hide(c(Df, `Sum Sq`)) %>% 
  gt::fmt_number(columns = 2:5, decimals = 3) %>% 
  fmt_scientific(columns = 6, decimals = 2) %>% 
  tab_style(style = list(
    cell_text(weight = "bold")),
    locations = cells_body(
      columns = 6,
      rows = `Pr(>F)` < .01
    )
  ) %>% 
  opt_table_font(
    font = list(
      google_font(name = "Merriweather"),
      "Cochin", "serif"
    )
  )



ggplot(part_uneduc_dat) +
  geom_point(aes(prop_uneduc, prop_part_uneduc, size = factor(scs), color = factor(scs)), alpha = .7) +
  theme_minimal()
