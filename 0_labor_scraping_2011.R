## 2011 census labor web scraping script

## load packages
library(tidyverse)
library(here)
library(magrittr)
library(rvest)
library(purrr)
library(pdftools)
library(stringr)


## create table of links to download data for each geo
url <- "https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/Rp-eng.cfm?TABID=4&LANG=E&A=R&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=01&GL=-1&GID="
url0 <- "&GK=1&GRP=1&O=D&PID=105611&PRID=0&PTYPE=105277&S=0&SHOWALL=0&SUB=0&Temporal=2013&THEME=96&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0"
url1 <- "&GK=1&GRP=1&O=D&PID=105611&PRID=0&PTYPE=105277&S=0&SHOWALL=0&SUB=0&Temporal=2013&THEME=96&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=1&D3=0&D4=0&D5=0&D6=0"
url2 <- "&GK=1&GRP=1&O=D&PID=105611&PRID=0&PTYPE=105277&S=0&SHOWALL=0&SUB=0&Temporal=2013&THEME=96&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=2&D3=0&D4=0&D5=0&D6=0"
url3 <- "&GK=1&GRP=1&O=D&PID=105611&PRID=0&PTYPE=105277&S=0&SHOWALL=0&SUB=0&Temporal=2013&THEME=96&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=3&D3=0&D4=0&D5=0&D6=0"
gid <- seq(from = 1118297, to = 1118464)
table <- tibble("gid" = vector(length = 0, mode = "numeric"), "link" = vector(length = 0, mode = "character"))

for(i in gid) {
  link <- paste0(url, i, url0)
  html <- read_html(link)
  download <- html %>% html_elements("li a") %>% html_attr("href") %>% 
    str_subset(pattern = "CSV$") %>% 
    str_remove_all("amp")
  output <- tibble("gid" = i, "link" = download)
  table <- bind_rows(table, output)
}
  # do this for each education level
for(i in gid) {
  link <- paste0(url, i, url1)
  html <- read_html(link)
  download <- html %>% html_elements("li a") %>% html_attr("href") %>% 
    str_subset(pattern = "CSV$") %>% 
    str_remove_all("amp")
  output <- tibble("gid" = i, "link" = download)
  table <- bind_rows(table, output)
}

for(i in gid) {
  link <- paste0(url, i, url2)
  html <- read_html(link)
  download <- html %>% html_elements("li a") %>% html_attr("href") %>% 
    str_subset(pattern = "CSV$") %>% 
    str_remove_all("amp")
  output <- tibble("gid" = i, "link" = download)
  table <- bind_rows(table, output)
}

for(i in gid) {
  link <- paste0(url, i, url3)
  html <- read_html(link)
  download <- html %>% html_elements("li a") %>% html_attr("href") %>% 
    str_subset(pattern = "CSV$") %>% 
    str_remove_all("amp")
  output <- tibble("gid" = i, "link" = download)
  table <- bind_rows(table, output)
}

## download each file to new table
  # create new directory
dir <- here(paste0("temp/"))
dir.create(dir)
  # create empty table
table2 <- tibble("V1" = vector(length = 0, mode = "character"), "V2" = vector(length = 0, mode = "character"), "V3" = vector(length = 0, mode = "character"),
                 "V4" = vector(length = 0, mode = "character"), "V5" = vector(length = 0, mode = "character"), "V6" = vector(length = 0, mode = "character"),
                 "V7" = vector(length = 0, mode = "character"), "V8" = vector(length = 0, mode = "character"))
gid <- table$gid
links <- table[,2]
  # download each file to new directory
for(i in 1:168) {
  
  url <- paste0("https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/", links[i,])
  raw_list <- download.file(url, paste0(dir, "a", gid[i], ".csv"))
}
  # repeat for each education level
for(i in 1:168) {
  
  url <- paste0("https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/", links[i,])
  raw_list <- download.file(url, paste0(dir, "b", gid[i], ".csv"))
}

for(i in 1:168) {
  
  url <- paste0("https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/", links[i,])
  raw_list <- download.file(url, paste0(dir, "c", gid[i], ".csv"))
}

for(i in 1:168) {
  
  url <- paste0("https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/", links[i,])
  raw_list <- download.file(url, paste0(dir, "d", gid[i], ".csv"))
}
  # download each csv, extract data to new table
for(i in 1:168) {
  output <- read.table(file = paste0("temp/", "a", gid[i], ".csv"), header = F, sep = ",",
                       col.names = paste0("V", seq_len(8)), fill = TRUE) %>% 
    slice(2, 8, 10)
  table2 <- bind_rows(table2, output)
}
  # repeat for each education level
for(i in 1:168) {
  output <- read.table(file = paste0("temp/", "b", gid[i], ".csv"), header = F, sep = ",",
                       col.names = paste0("V", seq_len(8)), fill = TRUE) %>% 
    slice(2, 8, 10)
  table2 <- bind_rows(table2, output)
}

for(i in 1:168) {
  output <- read.table(file = paste0("temp/", "c", gid[i], ".csv"), header = F, sep = ",",
                       col.names = paste0("V", seq_len(8)), fill = TRUE) %>% 
    slice(2, 8, 10)
  table2 <- bind_rows(table2, output)
}

for(i in 1:168) {
  output <- read.table(file = paste0("temp/", "d", gid[i], ".csv"), header = F, sep = ",",
                       col.names = paste0("V", seq_len(8)), fill = TRUE) %>% 
    slice(2, 8, 10)
  table2 <- bind_rows(table2, output)
}

## extracting geo names to accompany data
  # create odd row indicator
row_odd <- seq_len(nrow(table2)) %% 3 
  # subset odd rows
data_row_odd <- table2[row_odd == 1, 1]
  # remove unfavorable string elements
table2_names <- data_row_odd %>% 
  str_remove_all(pattern = "Geography = ") %>% 
  str_remove_all('\\\"') %>% 
  str_remove_all("c\\(") %>% 
  str_remove_all("\\)$") %>% 
  str_remove_all("\\[.\\]")
  # create df, making geo a column
colnames <- data.frame("names" = table2_names) %>% 
  separate(names, into = paste0("V", seq_len(672)), sep = ", ") %>% 
  t()

## tidy data with geo name, values, corrected col names
  # extract values
values <- table2 %>% 
  slice(which(row_number() %% 3 == 0))
  # add geo names
values$V1 <- colnames
  # correct col names
colnames <- c("geo_name", "total", "in_labor_force", "employed", "unemployed", "not_in_labor_force", "participation_rate", "unemployment_rate")

labor_2011 <- values
colnames(labor_2011) <- colnames
#rename(labor_2011, geo_name = `geo_name[,1]`)
  # not working
labor_2011$year <-  2011
labor_year$educ_level <-   
labor_2011 <- labor_2011 %>% 
  select(year, everything())
labor_2011 <- labor_2011 %>% 
  mutate(
    total = as.numeric(total),
    in_labor_force = as.numeric(in_labor_force),
    employed = as.numeric(employed),
    unemployed = as.numeric(unemployed),
    not_in_labor_force = as.numeric(not_in_labor_force),
    participation_rate = as.numeric(participation_rate),
    unemployment_rate = as.numeric(unemployment_rate)
  )
  