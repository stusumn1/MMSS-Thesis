## tidying income data 2011-2021

## load packages
library(tidyverse)
library(here)
library(magrittr)
library(rvest)
library(purrr)
library(pdftools)
library(stringr)

## read in data
income_2011 <- read_csv("data/raw/income_2011.csv") %>% 
  janitor::clean_names()
income_1621 <- read_csv("data/raw/income_1621.csv") %>% 
  janitor::clean_names()
