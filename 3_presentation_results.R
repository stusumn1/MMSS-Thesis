library(gt)
library(gtExtras)
library(tidyverse)

tbl <- tibble("Model" = c("Labor Force Participation", "Intact Families", "Low-Income Status"), "Effect Size" = c(.144, .04, -.12), SD = c(.02, .03, .06), "p-value" = c(.088, .071, .092))

tbl %>% 
  gt() %>% 
  gt_theme_nytimes()
