## 2000-2021 crime scraping

## load packages
library(tidyverse)
library(here)
library(magrittr)
library(rvest)
library(purrr)
library(pdftools)
library(stringr)

## create tables of links to download for each geo's crime data, 2000-2017
  # define urls from 2000-2017 by geo
url1 <- "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510002601&pickMembers%5B0%5D="
url2 <- "&cubeTimeFrame.startYear=2000&cubeTimeFrame.endYear=2021&referencePeriods=20170101%2C20210101"
gid <- paste0("1.", seq(from = 1, to = 50))

table <- tibble("gid" = vector(length = 0, mode = "numeric"), "link" = vector(length = 0, mode = "character"))

link <- paste0(url1, 1.2, url2)
link
