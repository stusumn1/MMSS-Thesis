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
load("data/processed/family_census_little.rda")

# feature engineering/recipe
family_recipe <- recipe(prop_two_parent ~ ., family_census) %>% 
  step_rm(geo, land_area, two_parent, one_parent) %>% 
  step_filter_missing(all_predictors(), threshold = .15) %>% 
  step_impute_knn(prop_two_parent) %>% 
  step_log(population, total_private_dwellings) %>% 
  #step_log(total_0_to_4, married, population_density) %>% 
  step_normalize(population_change, avg_household_size) %>% 
  step_zv() %>% 
  prep()

## proportion in two-parent families aged 0-4
family <- bake(family_recipe, new_data = NULL) %>% 
  mutate(scs = factor(scs))

# check out distributions
family %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") +
  geom_density() +
  theme_bw() +
  scale_x_continuous(breaks = NULL)

# run simple ANOVA
family_anova <- aov(prop_two_parent ~ . - scs, data = family)
family_anova %>% 
  summary()
family_tukey <- TukeyHSD(family_anova)
plot(family_tukey) 

family_summary <- data.frame(Covariate = c("year", "population", "population % change", "private dwellings","population density", "average age",  "average household size", "number married couples", "average family size", "total children aged 0-4", "residuals"), unclass(summary(family_anova)),  # Convert summary to data frame
                           check.names = F)
family_summary[1:10,] %>% 
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
  ) # should be modified to accurately reflect the variables I chose

ggplot(part_uneduc_dat) +
  geom_point(aes(prop_uneduc, prop_part_uneduc, size = factor(scs), color = factor(scs)), alpha = .7) +
  theme_minimal()
