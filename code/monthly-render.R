library(DBI)
library(RJDBC)
library(ggpubr)
library(rmarkdown)
library(lubridate)
library(magrittr)
library(openxlsx)
library(sida)
library(tidyverse)

connection_eis <- connect()

# Set parameters ----

month_folder <- "09"

month_for_ytd_filter <- 9

# Tidy and write data ----

source("code/schools.R")
source("code/monthly-eis-data.R")

# Do not source this file. Step through it, and resolve issues as they arise.
# source("code/monthly-teacher-data.R")

# Render reports ----

render(
  input = "code/monthly-report.Rmd"
  # params = list("data_file_date" = "2019-09-04")
)

# Clean up ----

rm(list = grep("schools_csi|districts_csi|school_ids", ls(), invert = T, value = T))
