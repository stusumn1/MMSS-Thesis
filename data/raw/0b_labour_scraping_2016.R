## 2016 census labor web scraping script

## load packages
library(tidyverse)
library(here)
library(magrittr)
library(rvest)
library(purrr)
library(pdftools)
library(stringr)


## create table of links to download data for each geo
url1 <- "https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/Rp-eng.cfm?TABID=4&LANG=E&A=R&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=01&GL=-1&GID="
url2 <- "&GK=1&GRP=1&O=D&PID=111335&PRID=10&PTYPE=109445&S=0&SHOWALL=0&SUB=0&Temporal=2017&THEME=123&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0"
gid <- seq(from = 1341680, to = 1341852)
table <- tibble("gid" = vector(length = 0, mode = "numeric"), "link" = vector(length = 0, mode = "character"))

for(i in gid) {
  link <- paste0(url1, 1341680, url2)
  html <- read_html(link)
  href <- html %>% html_elements("a") %>% html_attr("href") %>% 
    str_subset(pattern = "^#?") %>% 
    str_subset(pattern = "noSymbol.csv$")
  path <- paste0("https://www150.statcan.gc.ca/t1/tbl1/", href)
  output <- tibble("gid" = i, "link" = path)
  table <- bind_rows(table, output)
}
