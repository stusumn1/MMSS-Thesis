census
total_labor

## calculate education proportions----
test_total_educ <- total_labor %>% 
  filter(educ_level == "Total - Highest certificate, diploma or degree")
test_no_educ <- total_labor %>% 
  filter(educ_level == "Below High School")
educ_dat <- bind_cols(test_total_educ, test_no_educ[,3:10]) %>%
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
  select(1, 2, 19:25)

## combine with census data
labor_census_test <- full_join(educ_dat, census, by = c("year", "geo"))  

labor_census_test %>% 
  filter(!is.na(total)) %>% 
  view()

# another issue -- 2016 has messed up labor data compared to census, populations don't add up
# and a ton of missing values when we combine, lots that the labor has that census doesn't
# and vice versa