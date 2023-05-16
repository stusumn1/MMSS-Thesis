## tidying crime data 1998-2021

## load packages
library(tidyverse)
library(here)
library(magrittr)
library(rvest)
library(purrr)
library(pdftools)
library(stringr)

## read in data
crime_og <- read_csv("data/raw/crime_big.csv") %>% 
  janitor::clean_names()

  # correct names, remove ID and province
crime_og$geo <- crime_og$geo %>% 
  str_remove_all("\\[.*\\]") %>% 
  str_remove_all(", .* .*")
crime_og$geo[15] <- "Ottawa - Gatineau (Quebec part)"
crime_og$geo[17] <- "Ottawa - Gatineau"
crime_og$geo[18] <- "Ottawa - Gatineau (Ontario part)"
crime_og$geo

crime_og %>% 
  select()



