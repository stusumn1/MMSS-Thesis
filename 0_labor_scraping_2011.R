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
dir <- here(paste0("labor_files/"))
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
for(i in 169:336) {
  
  url <- paste0("https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/", links[i,])
  raw_list <- download.file(url, paste0(dir, "b", gid[i], ".csv"))
}

for(i in 337:504) {
  
  url <- paste0("https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/", links[i,])
  raw_list <- download.file(url, paste0(dir, "c", gid[i], ".csv"))
}

for(i in 505:672) {
  
  url <- paste0("https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/", links[i,])
  raw_list <- download.file(url, paste0(dir, "d", gid[i], ".csv"))
}
  # download each csv, extract data to new table
for(i in 1:168) {
  output <- read.table(file = paste0("labor_files/", "a", gid[i], ".csv"), header = F, sep = ",",
                       col.names = paste0("V", seq_len(8)), fill = TRUE) %>% 
    slice(2, 8, 10)
  table2 <- bind_rows(table2, output)
}
  # repeat for each education level
for(i in 169:336) {
  output <- read.table(file = paste0("labor_files/", "b", gid[i], ".csv"), header = F, sep = ",",
                       col.names = paste0("V", seq_len(8)), fill = TRUE) %>% 
    slice(2, 8, 10)
  table2 <- bind_rows(table2, output)
}

for(i in 337:504) {
  output <- read.table(file = paste0("labor_files/", "c", gid[i], ".csv"), header = F, sep = ",",
                       col.names = paste0("V", seq_len(8)), fill = TRUE) %>% 
    slice(2, 8, 10)
  table2 <- bind_rows(table2, output)
}

for(i in 505:672) {
  output <- read.table(file = paste0("labor_files/", "d", gid[i], ".csv"), header = F, sep = ",",
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
  str_remove_all("\\\n") %>% 
  str_remove_all("\\[1\\]")

  # create df, making geo a column
colnames <- data.frame("names" = table2_names) %>% 
  separate(names, into = paste0("V", seq_len(672)), sep = ", ") %>% 
  select(-568, -400, -232, -64) %>% 
  t()
rownames(colnames) <- 1:668

## correct flawed names manually ----
colnames[637] <- "Lloydminster (Alberta part)"
colnames[470] <- "Lloydminster (Alberta part)"
colnames[303] <- "Lloydminster (Alberta part)"
colnames[136] <- "Lloydminster (Alberta part)"

colnames[636] <- "Lloydminster (Saskatchewan part)"
colnames[472] <- "Lloydminster (Saskatchewan part)"
colnames[301] <- "Lloydminster (Saskatchewan part)"
colnames[135] <- "Lloydminster (Saskatchewan part)"

colnames[568] <- "Ottawa - Gatineau (Ontario part)"
colnames[401] <- "Ottawa - Gatineau (Ontario part)"
colnames[234] <- "Ottawa - Gatineau (Ontario part)"
colnames[67] <- "Ottawa - Gatineau (Ontario part)"

colnames[567] <- "Ottawa - Gatineau (Quebec part)"
colnames[400] <- "Ottawa - Gatineau (Quebec part)"
colnames[233] <- "Ottawa - Gatineau (Quebec part)"
colnames[66] <- "Ottawa - Gatineau (Quebec part)"

colnames[565] <- "Hawkesbury (Ontario part)"
colnames[398] <- "Hawkesbury (Ontario part)"
colnames[231] <- "Hawkesbury (Ontario part)"
colnames[64] <- "Hawkesbury (Ontario part)"

colnames[557] <- "Montreal"
colnames[390] <- "Montreal"
colnames[223] <- "Montreal"
colnames[56] <- "Montreal"

colnames[549] <- "Trois-Riveres"
colnames[382] <- "Trois-Riveres"
colnames[215] <- "Trois-Riveres"
colnames[48] <- "Trois-Riveres"

colnames[543] <- "Quebec"
colnames[376] <- "Quebec"
colnames[209] <- "Quebec"
colnames[42] <- "Quebec"

colnames[542] <- "Sept-Iles"
colnames[375] <- "Sept-Iles"
colnames[208] <- "Sept-Iles"
colnames[41] <- "Sept-Iles"

colnames[537] <- "Rivere-du-Loup"
colnames[370] <- "Rivere-du-Loup"
colnames[203] <- "Rivere-du-Loup"
colnames[36] <- "Rivere-du-Loup"

colnames[533] <- "Campbellton (Quebec part)"
colnames[366] <- "Campbellton (Quebec part)"
colnames[199] <- "Campbellton (Quebec part)"
colnames[32] <- "Campbellton (Quebec part)"

colnames[532] <- "Campbellton(New Brunswick)"
colnames[365] <- "Campbellton(New Brunswick)"
colnames[198] <- "Campbellton(New Brunswick)"
colnames[31] <- "Campbellton(New Brunswick)"

colnames[513] <- "Northwest Territories"
colnames[346] <- "Northwest Territories"
colnames[179] <- "Northwest Territories"
colnames[12] <- "Northwest Territories"

colnames[511] <- "British Columbia"
colnames[344] <- "British Columbia"
colnames[177] <- "British Columbia"
colnames[10] <- "British Columbia"

colnames[506] <- "Quebec"
colnames[339] <- "Quebec"
colnames[172] <- "Quebec"
colnames[5] <- "Quebec"

colnames[505] <- "New Brunswick"
colnames[338] <- "New Brunswick"
colnames[171] <- "New Brunswick"
colnames[4] <- "New Brunswick"

colnames[504] <- "Nova Scotia"
colnames[337] <- "Nova Scotia"
colnames[170] <- "Nova Scotia"
colnames[3] <- "Nova Scotia"

colnames[503] <- "Prince Edward Island"
colnames[336] <- "Prince Edward Island"
colnames[169] <- "Prince Edward Island"
colnames[2] <- "Prince Edward Island"

colnames[502] <- "Newfoundland and Labrador"
colnames[335] <- "Newfoundland and Labrador"
colnames[168] <- "Newfoundland and Labrador"
colnames[1] <- "Newfoundland and Labrador"

## tidy data with geo name, values, corrected col names ----
  # extract values
values <- table2 %>% 
  slice(which(row_number() %% 3 == 0)) %>% 
  filter(!is.na(V2))
  # add geo names
values$V1 <- colnames
  # correct col names
colnames <- c("geo_name", "total", "in_labor_force", "employed", "unemployed", "not_in_labor_force", "participation_rate", "unemployment_rate")

labor_2011 <- values
colnames(labor_2011) <- colnames
#rename(labor_2011, geo_name = `geo_name[,1]`)
  # not working
labor_2011$year <-  2011
labor_2011$educ_level <- c(rep("Total - Highest certificate, diploma or degree", 167), rep("Below High School", 167), rep("High School", 167), rep("Apprenticeship", 167))
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

save(labor_2011, file = "data/processed/labor_2011.rda")
