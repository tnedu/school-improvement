library(DBI)
library(RJDBC)
library(lubridate)
library(magrittr)
library(openxlsx)
library(sida)
library(tidyverse)

connection_eis <- connect()

# Set parameters ----

month_for_ytd_filter <- 8

# Tidy and write data ----

source("code/monthly-eis-data.R")

# Render reports ----

districts_csi <-
  read.xlsx(
    "C:/Users/CA20397/SharePoint/School Improvement - Documents/School Lists/school-designations-2018.xlsx",
    sheet = "Comprehensive Support"
  ) %>%
  filter(
    active == "Yes",
    !(system == 600 & school == 110), # Northfield Academy (adult high school)
    !(system == 792 & school == 8275) # The Excel Center (adult high school)
  ) %>%
  distinct(system) %>%
  arrange(system) %>%
  extract2("system")
